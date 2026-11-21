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

and 'a request = {
  operation : Raw.operation;
  id : int;
  data : 'a;
  owner : Raw.port;
  handle : 'a handle;
  receive_key : file_key option;
  mutable eof_on_cancel : bool;
  mutable immediate : bool;
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

let with_receives h receive fn =
  if receive then Mutex.protect h.association.guard fn else fn ()

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

let cancel t r =
  locked t (fun () ->
      check t;
      if r.owner != t.port then invalid_arg "Iocp.cancel: wrong port";
      match Hashtbl.find_opt t.requests r.id with
      | Some current when current == r -> Raw.cancel_operation r.operation
      | _ -> ())

let cancel_all t =
  locked t (fun () ->
      check t;
      Hashtbl.iter (fun _ r -> Raw.cancel_operation r.operation) t.requests)

let check_timeout timeout =
  if timeout < -1 || Int64.of_int timeout > 0xffff_fffeL then
    invalid_arg "Iocp: invalid timeout"

let untrack t r =
  Hashtbl.remove t.requests r.id;
  Option.iter (Hashtbl.remove r.handle.association.receives) r.receive_key

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
          with_receives r.handle (Option.is_some r.receive_key) (fun () ->
              let result = Some (Completion (r.data, Raw.completion r.operation r.eof_on_cancel)) in
          untrack t r;
          Atomic.decr r.handle.pending;
          Raw.acknowledge slot;
          result))

let submit_pinned ~receive t ~data h roots fn =
      let rec fresh_id () =
        t.next_id <- if t.next_id = max_int then 1 else t.next_id + 1;
        if Hashtbl.mem t.requests t.next_id then fresh_id () else t.next_id
      in
      let id = fresh_id () in
      let delivery =
        if h.association.port_state != t then
          (if t.skip_on_success then Raw.Forward_immediate else Raw.Forward_queue)
        else
          let skip = match h.association.skip with
            | Some skip -> skip
            | None ->
                let skip = Raw.enable_immediate h.fd in
                h.association.skip <- Some skip;
                skip
          in
          if not skip then Raw.Native_queue
          else if t.skip_on_success then Raw.Immediate
          else Raw.Post_queue
      in
      let operation = Raw.allocate t.port id h.fd (h, roots) delivery in
      let r =
        {
          operation;
          id;
          data;
          owner = t.port;
          handle = h;
          receive_key = (if receive then Some (object end) else None);
          eof_on_cancel = false;
          immediate = false;
        }
      in
      match
        Option.iter (fun key -> Hashtbl.add h.association.receives key r) r.receive_key;
        Hashtbl.add t.requests id r;
        fn operation
      with
      | immediate ->
          r.immediate <- immediate;
          if immediate then untrack t r;
          r
      | exception ex ->
          untrack t r;
          raise ex

let submit ?(receive = false) t ~data h roots fn =
  locked t (fun () ->
      check t;
      acquire h;
      match with_receives h receive (fun () -> submit_pinned ~receive t ~data h roots fn) with
      | r -> if r.immediate then Atomic.decr h.pending; r
      | exception ex -> Atomic.decr h.pending; raise ex)

let result r = if r.immediate then Some (Raw.completion r.operation false) else None

let buffer kind t ~data fd buf ~offset =
  if offset < 0L && not (kind = Raw.Write_file && offset = -1L) then
    invalid_arg "Iocp: negative offset";
  submit
    ~receive:(kind = Raw.Recv || kind = Raw.Recv_from)
    t ~data fd buf
    (fun op -> Raw.submit_buffer op kind buf offset)

let read t = buffer Raw.Read_file t
let write t = buffer Raw.Write_file t
let recv t ~data fd buf = buffer Raw.Recv t ~data fd buf ~offset:0L
let send t ~data fd buf = buffer Raw.Send t ~data fd buf ~offset:0L
let recvfrom t ~data fd buf = buffer Raw.Recv_from t ~data fd buf ~offset:0L
let sendto t ~data fd buf addr = buffer (Raw.Send_to addr) t ~data fd buf ~offset:0L

let accept t ~data fd ~client =
  submit t ~data fd client (fun op -> Raw.submit_accept op client)

let connect t ~data fd addr =
  submit t ~data fd addr (fun op -> Raw.submit_connect op addr)

let openfile path flags perm =
  let has f = List.mem f flags in
  List.iter
    (function
      | Unix.O_RDONLY | O_WRONLY | O_RDWR | O_APPEND | O_CREAT | O_TRUNC
      | O_EXCL | O_CLOEXEC | O_KEEPEXEC ->
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
  let fd =
    if access <> Raw.Read && has O_CREAT && perm land 0o200 = 0 then
      Raw.create_readonly path access (has O_APPEND) ~exclusive:(has O_EXCL)
        ~truncate:(has O_TRUNC)
    else
      let original = Unix.openfile path open_flags perm in
      Fun.protect
        ~finally:(fun () -> Unix.close original)
        (fun () -> Raw.reopen original access (has O_APPEND))
  in
  try
    if has O_KEEPEXEC && not (has O_CLOEXEC) then Unix.clear_close_on_exec fd;
    fd
  with ex ->
    Unix.close fd;
    raise ex

let pipe ?(read_async = true) ?(write_async = true) () =
  Raw.pipe_raw read_async write_async

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
            Hashtbl.iter
              (fun _ r ->
                r.eof_on_cancel <- true;
                Raw.cancel_operation r.operation)
              h.association.receives))

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
