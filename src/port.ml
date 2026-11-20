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
  mutable registered : int;
  mutable closed : bool;
  mutable waiting : bool;
}

and association = { port_state : t; mutable handles : handle list }

and handle = {
  association : association;
  fd : Unix.file_descr;
  mutable released : bool;
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
    closed = false;
    waiting = false;
  }

let close t =
  locked t (fun () ->
      if not t.closed then (
        if t.waiting || t.registered <> 0 then
          invalid_arg "Iocp.close: port is busy";
        Raw.close_port t.port;
        t.closed <- true))

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
          let h = { association = a; fd; released = false } in
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
      if not h.released then
        Mutex.protect association_lock (fun () ->
            let remaining = List.filter (fun other -> other != h) a.handles in
            if remaining = [] then (
              let all = List.filter (fun other -> other != a) !associations in
              Raw.detach h.fd;
              associations := all);
            a.handles <- remaining;
            h.released <- true;
            a.port_state.registered <- a.port_state.registered - 1))

let wake t =
  locked t (fun () ->
      check t;
      Raw.wake_port t.port)

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
  | _ -> assert false
