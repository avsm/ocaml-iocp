(*----------------------------------------------------------------------
   Copyright (c) 2022 Patrick Ferris <patrick@sirref.org>
   Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>
   Distributed under the MIT license. See terms at the end of this file.
  ----------------------------------------------------------------------*)

type completion = Raw.completion = {
  bytes_transferred : int;
  error : Unix.error option;
  source : Unix.sockaddr option;
}

type 'a event = Wake | Completion of 'a * completion

type waiter = { slot : Raw.slot; mutable busy : bool }

type file_key = < >

type cancellation = Not_requested | Explicit | Receive_shutdown

module Files = Map.Make (struct
  type t = file_key
  let compare = Stdlib.compare
end)

type 'a t = {
  port : Raw.port;
  skip_on_success : bool;
  lock : Mutex.t;
  mutable next_id : int;
  requests : (int, 'a request) Hashtbl.t;
  mutable files : 'a association Files.t;
  mutable closed : bool;
  mutable waiters : waiter list;
}

and 'a association = {
  key : file_key;
  port_state : 'a t;
  guard : Mutex.t;
  receives : (file_key, 'a request) Hashtbl.t;
  mutable skip : bool option;
  mutable handles : 'a handle list;
}

and 'a handle = {
  association : 'a association;
  fd : Unix.file_descr;
  pending : int Atomic.t; (* Negative once closed. *)
}

and 'a target = Registered of 'a handle | Event_fd of Unix.file_descr

and 'a request = {
  operation : Raw.operation;
  id : int;
  data : 'a;
  owner : Raw.port;
  target : 'a target;
  receive_key : file_key option;
  mutable cancellation : cancellation;
  mutable submission : Raw.submission;
}

let locked t fn = Mutex.protect t.lock fn
let check t = if t.closed then invalid_arg "Iocp: closed port"

let create ?(skip_on_success = false) () =
  {
    skip_on_success;
    port = Raw.create_port ();
    files = Files.empty;
    lock = Mutex.create ();
    next_id = 0;
    requests = Hashtbl.create 16;
    closed = false;
    waiters = [];
  }

let pending t = locked t (fun () -> Hashtbl.length t.requests)

let close t =
  locked t (fun () ->
      if not t.closed then (
        if List.exists (fun w -> w.busy) t.waiters || not (Files.is_empty t.files) || Hashtbl.length t.requests <> 0
        then invalid_arg "Iocp.close: port is busy";
        Raw.close_port t.port;
        t.closed <- true))

let check_handle t h =
  check t;
  if Atomic.get h.pending < 0 then invalid_arg "Iocp: closed handle";
  if h.association.port_state != t then invalid_arg "Iocp: wrong port"

let with_receives target receive fn =
  match target with
  | Registered h when receive -> Mutex.protect h.association.guard fn
  | _ -> fn ()

let file_key h = h.association.key
let same_handle = Raw.same_handle

let register t fd =
  locked t (fun () ->
      check t;
      let a =
        {
          key = object end;
          port_state = t;
          guard = Mutex.create ();
          receives = Hashtbl.create 0;
          skip = None;
          handles = [];
        }
      in
      let h = { association = a; fd; pending = Atomic.make 0 } in
      a.handles <- [ h ];
      let files = Files.add a.key a t.files in
      match Raw.associate_port t.port fd with
      | () ->
          t.files <- files;
          h
      | exception (Unix.Unix_error (EINVAL, _, _) as ex) -> (
          (* Windows rejects a second association, including through a duplicate. *)
          match
            Files.to_seq t.files
            |> Seq.find_map (fun (_, a) ->
                if Raw.same_handle fd (List.hd a.handles).fd then Some a
                else None)
          with
          | None -> raise ex
          | Some a ->
              let h = { h with association = a } in
              a.handles <- h :: a.handles;
              h))

let close_handle h =
  let a = h.association in
  let t = a.port_state in
  locked t (fun () ->
      if Atomic.get h.pending >= 0 then (
        let remaining = List.filter (fun other -> other != h) a.handles in
        let files =
          if remaining = [] then Files.remove a.key t.files else t.files
        in
        if not (Atomic.compare_and_set h.pending 0 (-1)) then
          invalid_arg "Iocp.close_handle: handle is busy";
        t.files <- files;
        a.handles <- remaining;
        if remaining = [] then Raw.detach h.fd))

(* A submission on another port can race with closing this registration. *)
let rec acquire h =
  let n = Atomic.get h.pending in
  if n < 0 then invalid_arg "Iocp: closed handle";
  if not (Atomic.compare_and_set h.pending n (n + 1)) then acquire h

let wake t =
  locked t (fun () ->
      check t;
      Raw.wake_port t.port)

let request_cancel reason r =
  Raw.cancel_operation r.operation;
  if r.cancellation = Not_requested then r.cancellation <- reason

let explicit_cancel r =
  with_receives r.target (Option.is_some r.receive_key) (fun () -> request_cancel Explicit r)

let cancel t r =
  locked t (fun () ->
      check t;
      if r.owner != t.port then invalid_arg "Iocp.cancel: wrong port";
      match Hashtbl.find_opt t.requests r.id with
      | Some current when current == r -> explicit_cancel r
      | _ -> ())

let cancel_all t =
  locked t (fun () ->
      check t;
      Hashtbl.iter (fun _ r -> explicit_cancel r) t.requests)

let check_timeout timeout =
  if timeout < -1 || Int64.of_int timeout > 0xffff_fffeL then
    invalid_arg "Iocp: invalid timeout"

let untrack t r =
  Hashtbl.remove t.requests r.id;
  match r.target, r.receive_key with
  | Registered h, Some key -> Hashtbl.remove h.association.receives key
  | _ -> ()

let release_target = function
  | Registered h -> Atomic.decr h.pending
  | Event_fd _ -> ()

let wait t ~timeout =
  check_timeout timeout;
  let claimed = ref None in
  let release () = Option.iter (fun w -> w.busy <- false) !claimed in
  Fun.protect ~finally:(fun () -> locked t release) @@ fun () ->
  let slot = locked t (fun () ->
      check t;
      (* A failed delivery keeps its packet in the slot for the next waiter. *)
      let rec choose idle = function
        | w :: rest when w.busy -> choose idle rest
        | w :: _ when w.slot.{0} >= 0 -> w
        | w :: rest -> choose (Some w) rest
        | [] -> (match idle with
            | Some w -> w
            | None ->
              let w = { slot = Raw.slot (); busy = false } in
              t.waiters <- w :: t.waiters;
              w)
      in
      let w = choose None t.waiters in
      claimed := Some w;
      w.busy <- true;
      w.slot)
  in
  match Raw.dequeue t.port slot timeout with
  | -1 -> None
  | 0 ->
      Raw.acknowledge slot;
      Some Wake
  | id ->
      locked t (fun () ->
          let r = Hashtbl.find t.requests id in
          with_receives r.target (Option.is_some r.receive_key) (fun () ->
              let result = Some (Completion (r.data, Raw.completion r.operation (r.cancellation = Receive_shutdown))) in
              untrack t r;
              release_target r.target;
              Raw.acknowledge slot;
              result))

let submit_pinned ~receive t ~data target roots fn =
  let rec fresh_id () =
    t.next_id <- if t.next_id = max_int then 1 else t.next_id + 1;
    if Hashtbl.mem t.requests t.next_id then fresh_id () else t.next_id
  in
  let id = fresh_id () in
  let event_delivery () =
    if t.skip_on_success then Raw.Forward_immediate else Raw.Forward_queue
  in
  let fd, delivery = match target with
    | Event_fd fd -> fd, event_delivery ()
    | Registered h when h.association.port_state != t -> h.fd, event_delivery ()
    | Registered h ->
      let skip = match h.association.skip with
        | Some skip -> skip
        | None ->
          let skip = Raw.enable_immediate h.fd in
          h.association.skip <- Some skip;
          skip
      in
      h.fd, (if not skip then Raw.Native_queue
             else if t.skip_on_success then Raw.Immediate else Raw.Post_queue)
  in
  let operation = Raw.allocate t.port id fd (target, roots) delivery in
  let r =
    {
      operation;
      id;
      data;
      owner = t.port;
      target;
      receive_key = (if receive then Some (object end) else None);
      cancellation = Not_requested;
      submission = Queued;
    }
  in
  match
    (match target, r.receive_key with
     | Registered h, Some key -> Hashtbl.add h.association.receives key r
     | _ -> ());
    Hashtbl.add t.requests id r;
    fn operation
  with
  | submission ->
      r.submission <- submission;
      if submission = Completed then untrack t r;
      r
  | exception ex ->
      untrack t r;
      raise ex

let submit ?(receive = false) t ~data target roots fn =
  let r = locked t (fun () ->
      check t;
      (match target with Registered h -> acquire h | Event_fd _ -> ());
      match with_receives target receive (fun () -> submit_pinned ~receive t ~data target roots fn) with
      | r -> if r.submission = Completed then release_target target; r
      | exception ex -> release_target target; raise ex)
  in
  if r.submission = Unrouted then (
    (* Cancellation may wait for the driver. Keep the request pinned while
       letting other submissions and consumers use the port. *)
    let release () =
      locked t (fun () ->
          with_receives target receive (fun () ->
              untrack t r;
              release_target target))
    in
    match Raw.drain_unrouted r.operation with
    | submission ->
        r.submission <- submission;
        if submission = Completed then release ()
    | exception ex -> release (); raise ex);
  r

let result r = if r.submission = Completed then Some (Raw.completion r.operation false) else None

let buffer kind t ~data fd buf ~offset =
  if offset < 0L && not (kind = Raw.Write_file && offset = -1L) then
    invalid_arg "Iocp: negative offset";
  submit
    ~receive:(kind = Raw.Recv || kind = Raw.Recv_from)
    t ~data fd buf
    (fun op -> Raw.submit_buffer op kind buf offset)

module Operations (H : sig
  type 'a t
  val target : 'a t -> 'a target
end) = struct
  let read t ~data fd = buffer Raw.Read_file t ~data (H.target fd)
  let write t ~data fd = buffer Raw.Write_file t ~data (H.target fd)
  let recv t ~data fd buf = buffer Raw.Recv t ~data (H.target fd) buf ~offset:0L
  let send t ~data fd buf = buffer Raw.Send t ~data (H.target fd) buf ~offset:0L
  let recvfrom t ~data fd buf = buffer Raw.Recv_from t ~data (H.target fd) buf ~offset:0L
  let sendto t ~data fd buf addr = buffer (Raw.Send_to addr) t ~data (H.target fd) buf ~offset:0L

  let accept t ~data fd ~client =
    submit t ~data (H.target fd) client (fun op -> Raw.submit_accept op client)

  let connect t ~data fd addr =
    submit t ~data (H.target fd) addr (fun op -> Raw.submit_connect op addr)
end

include Operations (struct
  type 'a t = 'a handle
  let target h = Registered h
end)

module Event = struct
  include Operations (struct
    type 'a t = Unix.file_descr
    let target fd = Event_fd fd
  end)

  let cancel_receives t fd =
    locked t (fun () ->
        if Hashtbl.length t.requests <> 0 then (
          let aliases = Hashtbl.create 8 in
          let matches other =
            fd == other ||
            match Hashtbl.find_opt aliases other with
            | Some same -> same
            | None ->
                let same = same_handle fd other in
                Hashtbl.add aliases other same;
                same
          in
          Hashtbl.iter (fun _ r ->
              match r.target, r.receive_key with
              | Event_fd other, Some _ when matches other -> request_cancel Receive_shutdown r
              | _ -> ()) t.requests))
end

let openfile path flags perm =
  let has f = List.mem f flags in
  List.iter
    (function
      | Unix.O_RDONLY | O_WRONLY | O_RDWR | O_APPEND | O_CREAT | O_TRUNC
      | O_EXCL | O_CLOEXEC | O_KEEPEXEC | O_SHARE_DELETE ->
          ()
      | _ -> invalid_arg "Iocp.openfile: unsupported flag")
    flags;
  let mode, access =
    if has Unix.O_RDWR then (Unix.O_RDWR, Raw.Read_write)
    else if has O_WRONLY then (Unix.O_WRONLY, Raw.Write)
    else (Unix.O_RDONLY, Raw.Read)
  in
  let open_flags =
    List.filter
      (function
        | Unix.O_RDONLY | O_WRONLY | O_RDWR | O_APPEND | O_CLOEXEC | O_KEEPEXEC
          ->
            false
        | _ -> true)
      flags
  in
  let open_flags = Unix.O_CLOEXEC :: mode :: open_flags in
  let append = has O_APPEND and share_delete = has O_SHARE_DELETE in
  let fd =
    if access <> Raw.Read && has O_CREAT && perm land 0o200 = 0 then
      let disposition =
        if has O_EXCL then Raw.Create_new else if has O_TRUNC then Create_always else Open_always
      in
      Raw.create_readonly path access ~append ~share_delete disposition
    else
      let original = Unix.openfile path open_flags perm in
      Fun.protect
        ~finally:(fun () -> Unix.close original)
        (fun () -> Raw.reopen original access ~append ~share_delete)
  in
  try
    if has O_KEEPEXEC && not (has O_CLOEXEC) then Unix.clear_close_on_exec fd;
    fd
  with ex ->
    Unix.close fd;
    raise ex

let pipe ?(read_async = true) ?(write_async = true) ?(message = false) () =
  Raw.pipe_raw read_async write_async ~message

let shutdown t h cmd =
  locked t (fun () ->
      Mutex.protect h.association.guard (fun () ->
          check_handle t h;
          (try Unix.shutdown h.fd cmd
           with Unix.Unix_error (ENOTCONN, _, _) as ex ->
             (* A duplicated Winsock descriptor can lack the connection context. *)
             let rec retry = function
               | [] -> raise ex
               | other :: rest when other.fd == h.fd -> retry rest
               | other :: rest -> (
                   try Unix.shutdown other.fd cmd
                   with Unix.Unix_error (ENOTCONN, _, _) -> retry rest)
             in
             retry h.association.handles);
          if cmd <> Unix.SHUTDOWN_SEND then
            Hashtbl.iter (fun _ r -> request_cancel Receive_shutdown r) h.association.receives))

module Shared = struct
  type 'a t = { raw : 'a; users : int Atomic.t; dispose : 'a -> unit }

  (* One reference belongs to the owner. Closing sets the sign bit and drops
     that reference after posting a notification for every possible waiter. *)
  let create raw dispose = { raw; users = Atomic.make 1; dispose }

  let rec acquire t =
    let n = Atomic.get t.users in
    n > 0 && (Atomic.compare_and_set t.users n (n + 1) || acquire t)

  let release t =
    if Atomic.fetch_and_add t.users (-1) = min_int + 1 then t.dispose t.raw

  let use t fn =
    if acquire t then
      match fn t.raw with
      | () -> release t
      | exception ex ->
          release t;
          raise ex

  let rec close t notify =
    let n = Atomic.get t.users in
    if n > 0 then
      if Atomic.compare_and_set t.users n (min_int + n) then (
        match notify t.raw (n - 1) with
        | () -> release t
        | exception ex ->
            release t;
            raise ex)
      else close t notify
end

module Wakeup = struct
  type t = Raw.port Shared.t

  let create () = Shared.create (Raw.create_port ()) Raw.close_port
  let signal t = Shared.use t Raw.wake_port

  let wait t timeout =
    check_timeout timeout;
    Shared.use t (fun p -> Raw.wait_notification p timeout)

  let close t =
    Shared.close t (fun p n ->
        for _ = 1 to n do
          Raw.wake_port p
        done)
end

module Interrupt = struct
  type t = Unix.file_descr Shared.t

  let current () = Shared.create (Raw.current_thread ()) Unix.close
  let signal t = Shared.use t Raw.interrupt
  let close t = Shared.close t (fun _ _ -> ())
end

(*---------------------------------------------------------------------------
  Copyright (c) 2022 <patrick@sirref.org>

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.
  ---------------------------------------------------------------------------*)
