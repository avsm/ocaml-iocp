(*----------------------------------------------------------------------
   Copyright (c) 2022 Patrick Ferris <patrick@sirref.org>
   Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>
   Distributed under the MIT license. See terms at the end of this file.
  ----------------------------------------------------------------------*)

type completion = Raw.completion = {
  id : int;
  bytes_transferred : int;
  error : Unix.error option;
  source : Unix.sockaddr option;
}

type t = {
  port : Raw.port;
  skip_on_success : bool;
  lock : Mutex.t;
  requests : (int, request) Hashtbl.t;
  mutable registered : int;
  mutable closed : bool;
  mutable waiting : bool;
}

and association = {
  port_state : t;
  mutable skip : bool option;
  mutable handles : handle list;
}

and handle = {
  association : association;
  fd : Unix.file_descr;
  mutable pending : int;
  mutable released : bool;
}

and request = {
  operation : Raw.operation;
  id : int;
  owner : Raw.port;
  handle : handle;
  receive : bool;
  mutable eof_on_cancel : bool;
  mutable immediate : bool;
}

let association_lock = Mutex.create ()
let associations : association list ref = ref []
let locked t fn = Mutex.protect t.lock fn
let check t = if t.closed then invalid_arg "Iocp: closed port"

let create ?(skip_on_success = false) () =
  {
    skip_on_success;
    port = Raw.create_port ();
    registered = 0;
    lock = Mutex.create ();
    requests = Hashtbl.create 16;
    closed = false;
    waiting = false;
  }

let pending t = locked t (fun () -> Hashtbl.length t.requests)

let close t =
  locked t (fun () ->
      if not t.closed then (
        if t.waiting || t.registered <> 0 || Hashtbl.length t.requests <> 0 then
          invalid_arg "Iocp.close: port is busy";
        Raw.close_port t.port;
        t.closed <- true))

let check_handle t h =
  check t;
  if h.released then invalid_arg "Iocp: closed handle";
  if h.association.port_state != t then invalid_arg "Iocp: wrong port"

let register t fd =
  locked t (fun () ->
      check t;
      Mutex.protect association_lock (fun () ->
          let found =
            List.find_opt
              (fun a -> Raw.same_handle fd (List.hd a.handles).fd)
              !associations
          in
          let a =
            match found with
            | Some a when a.port_state == t -> a
            | Some _ -> raise (Unix.Unix_error (EINVAL, "Iocp.register", ""))
            | None -> { port_state = t; skip = None; handles = [] }
          in
          let h = { association = a; fd; pending = 0; released = false } in
          let handles = h :: a.handles in
          (match found with
          | Some _ -> ()
          | None ->
              let all = a :: !associations in
              Raw.associate_port t.port fd;
              associations := all);
          a.handles <- handles;
          t.registered <- t.registered + 1;
          h))

let close_handle h =
  let a = h.association in
  locked a.port_state (fun () ->
      if not h.released then (
        if h.pending <> 0 then invalid_arg "Iocp.close_handle: handle is busy";
        Mutex.protect association_lock (fun () ->
            let remaining = List.filter (fun other -> other != h) a.handles in
            if remaining = [] then (
              let all = List.filter (fun other -> other != a) !associations in
              Raw.detach h.fd;
              associations := all);
            a.handles <- remaining;
            h.released <- true;
            a.port_state.registered <- a.port_state.registered - 1)))

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

let notification =
  Some { id = 0; bytes_transferred = 0; error = None; source = None }

let wait t ~timeout =
  if timeout < -1 || Int64.of_int timeout > 0xffff_fffeL then
    invalid_arg "Iocp.wait: timeout";
  let claimed = ref false in
  let release () = t.waiting <- false in
  Fun.protect ~finally:(fun () -> if !claimed then locked t release)
  @@ fun () ->
  locked t (fun () ->
      check t;
      if t.waiting then invalid_arg "Iocp.wait: concurrent consumer";
      t.waiting <- true;
      claimed := true);
  match Raw.dequeue t.port timeout with
  | -1 -> None
  | 0 ->
      Raw.acknowledge t.port;
      notification
  | id ->
      locked t (fun () ->
          let r = Hashtbl.find t.requests id in
          let result = Raw.completion r.operation r.eof_on_cancel in
          Hashtbl.remove t.requests id;
          r.handle.pending <- r.handle.pending - 1;
          Raw.acknowledge t.port;
          result)

let submit ?(receive = false) t ~id h roots fn =
  locked t (fun () ->
      check_handle t h;
      if id <= 0 || Hashtbl.mem t.requests id then
        invalid_arg "Iocp: duplicate or non-positive request ID";
      let skip =
        match h.association.skip with
        | Some skip -> skip
        | None ->
            let skip = Raw.enable_immediate h.fd in
            h.association.skip <- Some skip;
            skip
      in
      let delivery =
        if not skip then Raw.Native_queue
        else if t.skip_on_success then Raw.Immediate
        else Raw.Post_queue
      in
      let operation = Raw.allocate t.port id h.fd (h, roots) delivery in
      let r =
        {
          operation;
          id;
          owner = t.port;
          handle = h;
          receive;
          eof_on_cancel = false;
          immediate = false;
        }
      in
      h.pending <- h.pending + 1;
      match
        Hashtbl.add t.requests id r;
        fn operation
      with
      | immediate ->
          r.immediate <- immediate;
          if immediate then (
            h.pending <- h.pending - 1;
            Hashtbl.remove t.requests id);
          r
      | exception ex ->
          h.pending <- h.pending - 1;
          Hashtbl.remove t.requests id;
          raise ex)

let result r = if r.immediate then Raw.completion r.operation false else None

let buffer kind t ~id fd buf ~offset =
  if offset < 0L && not (kind = Raw.Write_file && offset = -1L) then
    invalid_arg "Iocp: negative offset";
  submit
    ~receive:(kind = Raw.Recv || kind = Raw.Recv_from)
    t ~id fd buf
    (fun op -> Raw.submit_buffer op kind buf offset)

let read = buffer Raw.Read_file
let write = buffer Raw.Write_file
let recv t ~id fd buf = buffer Raw.Recv t ~id fd buf ~offset:0L
let send t ~id fd buf = buffer Raw.Send t ~id fd buf ~offset:0L
let recvfrom t ~id fd buf = buffer Raw.Recv_from t ~id fd buf ~offset:0L
let sendto t ~id fd buf addr = buffer (Raw.Send_to addr) t ~id fd buf ~offset:0L

let accept t ~id fd ~client =
  submit t ~id fd client (fun op -> Raw.submit_accept op client)

let connect t ~id fd addr =
  submit t ~id fd addr (fun op -> Raw.submit_connect op addr)

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
            if r.receive && r.handle.association == h.association then (
              r.eof_on_cancel <- true;
              Raw.cancel_operation r.operation))
          t.requests)

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
