(*----------------------------------------------------------------------
   Copyright (c) 2022 Patrick Ferris <patrick@sirref.org>
   Distributed under the MIT license. See terms at the end of this file.
  ----------------------------------------------------------------------*)

module Wsabuf = Wsabuf
module Sockaddr = Sockaddr
module Handle = Handle
module Overlapped = Overlapped
module Raw = Raw

module Id = struct
  type t = int
  let compare : t -> t -> int = Int.compare
  let hash : t -> int = Hashtbl.hash
  let equal : t -> t -> bool = Int.equal
  let to_int (a:int) = a
end

type fd = Handle.t

module H = Hashtbl.Make(Id)

(* Values that must stay rooted for the lifetime of an in-flight operation. *)
type roots =
  | ReadWrite of Cstruct.buffer
  | RecvSend of Wsabuf.t
  | RecvSendAddr of Wsabuf.t * Sockaddr.t
  | Connect of Sockaddr.t

(* A [t] is owned by a single domain: submission, reaping and cancellation must
   all happen on that domain. *)
type t = {
  iocp : Raw.t;
  mutable next_id : int;                  (* Monotonic source of operation ids. *)
  in_flight : (Handle.t * int Overlapped.t * roots) H.t;  (* Keyed by op id. *)
  mutable free : int Overlapped.t list;   (* Pool of unused OVERLAPPEDs. *)
  mutable closed : bool;
}

type completion_status = {
  bytes_transferred : int;
  id : Id.t;
  error : Unix.error option;
}

(* A packet dequeued from the port: either a real overlapped I/O completion, or
   a key-only packet posted with {!post}/{!wakeup} or by a {!register_wait}. *)
type packet =
  | Io of completion_status
  | Posted of { key : int; bytes_transferred : int }

let handle_of_fd v fd key =
  Raw.associate_fd_with_iocp v.iocp fd key

let openfile t = Raw.openfile t.iocp

let get_overlapped v fd root =
  match v.free with
  | [] -> None
  | ol :: rest ->
    let id = v.next_id in
    v.next_id <- id + 1;
    Overlapped.set_key ol id;
    v.free <- rest;
    H.replace v.in_flight id (fd, ol, root);
    Some (ol, id)

(* Hand an OVERLAPPED back to the pool. Only ever for an operation that did not
   start: one that did is released by {!wait} when its completion arrives. *)
let release v id =
  match H.find_opt v.in_flight id with
  | None -> ()
  | Some (_, ol, _) -> H.remove v.in_flight id; v.free <- ol :: v.free

(* Take a slot from the pool and start an operation on it. If [start] raises,
   the operation never went pending, so no completion will ever arrive to
   release the slot and it has to go back here — otherwise every synchronous
   failure costs the port one OVERLAPPED for good. *)
let submit v fd root start =
  match get_overlapped v fd root with
  | None -> None
  | Some (ol, id) ->
    match start ol with
    | () -> Some id
    | exception ex -> release v id; raise ex

let read v fd buf ~pos ~len ~off =
  submit v fd (ReadWrite buf) (fun ol ->
      Overlapped.set_offset ol off;
      Raw.read fd buf len pos ol)

let write v fd buf ~pos ~len ~off =
  submit v fd (ReadWrite buf) (fun ol ->
      Overlapped.set_offset ol off;
      Raw.write fd buf len pos ol)

let wait v ~timeout =
  match Raw.get_queued_completion_status v.iocp timeout with
  | Raw.Cs_none -> None
  | Raw.Cs_some cs ->
    if cs.overlapped_id = 0 then
      (* No OVERLAPPED: a packet posted by [post]/[wakeup] or a registered wait. *)
      Some (Posted { key = cs.handle_id; bytes_transferred = cs.bytes_transferred })
    else begin
      let id = Overlapped.unsafe_key cs.overlapped_id in
      (match H.find_opt v.in_flight id with
       | Some (_, ol, _) -> H.remove v.in_flight id; v.free <- ol :: v.free
       | None -> ());
      Some (Io { bytes_transferred = cs.bytes_transferred; id; error = cs.error })
    end

(* Number of operations submitted but not yet reaped (including cancelled ones
   whose aborted completion has not yet been drained). *)
let active_ops v = H.length v.in_flight

(* Cancel an in-flight operation by its id. The cancelled operation still posts
   an [ERROR_OPERATION_ABORTED] completion that {!completion_status} reaps and
   recycles; an unknown / already-completed id is a no-op. *)
let cancel v id =
  match H.find_opt v.in_flight id with
  | Some (fd, ol, _) -> Raw.cancel fd ol
  | None -> ()

let cancel_all _v fd = Raw.cancel_all fd

let accept v sock sock_accept addr_buf =
  let buf = Cstruct.to_bigarray addr_buf in
  submit v sock (ReadWrite buf) (fun ol -> Raw.accept sock sock_accept buf ol)

let recv v sock bufs =
  let wsabuf = Wsabuf.create bufs in
  submit v sock (RecvSend wsabuf) (fun ol -> Raw.recv sock wsabuf ol)

let send v sock bufs =
  let wsabuf = Wsabuf.create bufs in
  submit v sock (RecvSend wsabuf) (fun ol -> Raw.send sock wsabuf ol)

(* [addr] is filled in with the source on completion, so it must stay alive. *)
let recv_from v sock bufs addr =
  let wsabuf = Wsabuf.create bufs in
  submit v sock (RecvSendAddr (wsabuf, addr)) (fun ol -> Raw.recv_from sock wsabuf addr ol)

let send_to v sock bufs addr =
  let wsabuf = Wsabuf.create bufs in
  submit v sock (RecvSendAddr (wsabuf, addr)) (fun ol -> Raw.send_to sock wsabuf addr ol)

let connect v sock addr =
  submit v sock (Connect addr) (fun ol -> Raw.connect sock addr ol)

(* Post a key-only packet to wake a thread in {!completion_status}. *)
let post v ~key ~bytes = Raw.post v.iocp key bytes
let wakeup v = Raw.post v.iocp 0 0

type wait = { mutable token : nativeint }

let register_wait v handle ~key = { token = Raw.register_wait v.iocp handle key }

let unregister_wait w =
  if w.token <> 0n then begin
    Raw.unregister_wait w.token;
    w.token <- 0n
  end

let shutdown _v fd cmd = Raw.shutdown fd cmd
let update_accept_ctx _v ~listen accept = Raw.update_accept_ctx accept listen
let update_connect_ctx _v fd = Raw.update_connect_ctx fd

(* Cancel everything still in flight and reap the aborted completions, so the
   kernel is no longer writing into any of the buffers we were keeping alive. *)
let drain v =
  H.iter (fun _ (fd, ol, _) -> Raw.cancel fd ol) v.in_flight;
  let rec loop () =
    if H.length v.in_flight = 0 then ()
    else match wait v ~timeout:100 with
      | None -> ()      (* give up: nothing more is going to arrive *)
      | Some _ -> loop ()
  in
  loop ()

let close v =
  if not v.closed then begin
    v.closed <- true;
    drain v;
    Raw.close_io_completion_port v.iocp
  end

let create ?(overlapped = 1024) n =
  let v =
    { iocp = Raw.create_io_completion_port n
    ; next_id = 1
    ; in_flight = H.create 255
    ; free = List.init overlapped (fun _ -> Overlapped.create 0)
    ; closed = false }
  in
  Gc.finalise close v;
  v

(*---------------------------------------------------------------------------
  Copyright (c) 2022 <patrick@sirref.org>
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>

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
