(* Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>
   SPDX-License-Identifier: MIT *)

type completion = Raw.completion = {
  id : int;
  bytes_transferred : int;
  error : Unix.error option;
}

type t = {
  port : Raw.port;
  lock : Mutex.t;
  requests : (int, request) Hashtbl.t;
  mutable registered : int;
  mutable closed : bool;
  mutable waiting : bool;
}

and association = { port_state : t; mutable handles : handle list }

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
}

let association_lock = Mutex.create ()
let associations : association list ref = ref []
let locked t fn = Mutex.protect t.lock fn
let check t = if t.closed then invalid_arg "Iocp: closed port"

let create () =
  {
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
            | None -> { port_state = t; handles = [] }
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

let notification = Some { id = 0; bytes_transferred = 0; error = None }

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
          let result = Raw.completion r.operation in
          Hashtbl.remove t.requests id;
          r.handle.pending <- r.handle.pending - 1;
          Raw.acknowledge t.port;
          result)

let submit t ~id h roots fn =
  locked t (fun () ->
      check_handle t h;
      if id <= 0 || Hashtbl.mem t.requests id then
        invalid_arg "Iocp: duplicate or non-positive request ID";
      let operation = Raw.allocate t.port id h.fd (h, roots) in
      let r = { operation; id; owner = t.port; handle = h } in
      h.pending <- h.pending + 1;
      match
        Hashtbl.add t.requests id r;
        fn operation
      with
      | () -> r
      | exception ex ->
          h.pending <- h.pending - 1;
          Hashtbl.remove t.requests id;
          raise ex)

let buffer kind t ~id fd buf ~offset =
  if offset < 0L && not (kind = Raw.Write_file && offset = -1L) then
    invalid_arg "Iocp: negative offset";
  submit t ~id fd buf (fun op -> Raw.submit_buffer op kind buf offset)

let read = buffer Raw.Read_file
let write = buffer Raw.Write_file
