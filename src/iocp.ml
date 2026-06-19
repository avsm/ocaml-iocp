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

(* A [t] is owned by a single domain. *)
type t = {
  iocp : Raw.t;
  mutable next_id : int;                  (* Monotonic source of operation ids. *)
  in_flight : (Handle.t * int Overlapped.t * roots) H.t;  (* Keyed by op id. *)
  mutable free : int Overlapped.t list;   (* Pool of unused OVERLAPPEDs. *)
}

type completion_status = {
  bytes_transferred : int;
  id : Id.t;
  error : Unix.error option;
}

type packet =
  | Io of completion_status
  | Posted of { key : int; bytes_transferred : int }

let handle_of_fd v fd key =
  Raw.associate_fd_with_iocp v.iocp fd key

let openfile t = Raw.openfile t.iocp

let with_overlapped v fd root submit =
  match v.free with
  | [] -> None
  | ol :: rest ->
    Overlapped.reset ol;
    let id = v.next_id in
    v.next_id <- id + 1;
    Overlapped.set_key ol id;
    v.free <- rest;
    H.replace v.in_flight id (fd, ol, root);
    begin match submit ol with
    | () -> Some id
    | exception e -> H.remove v.in_flight id; v.free <- ol :: v.free; raise e
    end

let read v fd buf ~pos ~len ~off =
  with_overlapped v fd (ReadWrite buf) (fun ol ->
      Overlapped.set_offset ol off; Raw.read fd buf len pos ol)

let write v fd buf ~pos ~len ~off =
  with_overlapped v fd (ReadWrite buf) (fun ol ->
      Overlapped.set_offset ol off; Raw.write fd buf len pos ol)

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

let active_ops v = H.length v.in_flight

let cancel v id =
  match H.find_opt v.in_flight id with
  | Some (fd, ol, _) -> Raw.cancel fd ol
  | None -> ()

let cancel_all _v fd = Raw.cancel_all fd

let accept v sock sock_accept addr_buf =
  let buf = Cstruct.to_bigarray addr_buf in
  with_overlapped v sock (ReadWrite buf) (fun ol -> Raw.accept sock sock_accept buf ol)

let recv v sock bufs =
  let wsabuf = Wsabuf.create bufs in
  with_overlapped v sock (RecvSend wsabuf) (fun ol -> Raw.recv sock wsabuf ol)

let send v sock bufs =
  let wsabuf = Wsabuf.create bufs in
  with_overlapped v sock (RecvSend wsabuf) (fun ol -> Raw.send sock wsabuf ol)

let recv_from v sock bufs addr =
  let wsabuf = Wsabuf.create bufs in
  with_overlapped v sock (RecvSendAddr (wsabuf, addr)) (fun ol -> Raw.recv_from sock wsabuf addr ol)

let send_to v sock bufs addr =
  let wsabuf = Wsabuf.create bufs in
  with_overlapped v sock (RecvSendAddr (wsabuf, addr)) (fun ol -> Raw.send_to sock wsabuf addr ol)

let connect v sock addr =
  with_overlapped v sock (Connect addr) (fun ol -> Raw.connect sock addr ol)

(* Post a key-only packet to wake a thread in {!wait}. Key 0 is the
   "just wake up" sentinel. TODO avsm: make this a proper type? *)
let post v ~key ~bytes = Raw.post v.iocp key bytes
let wakeup v = Raw.post v.iocp 0 0

type wait = { mutable token : nativeint }

let unregister_wait w =
  if w.token <> 0n then begin
    Raw.unregister_wait w.token;
    w.token <- 0n
  end

let register_wait v handle ~key =
  let w = { token = Raw.register_wait v.iocp handle key } in
  Gc.finalise (fun w -> unregister_wait w; ignore (Sys.opaque_identity v)) w;
  w

let shutdown _v fd cmd = Raw.shutdown fd cmd
let update_accept_ctx _v ~listen accept = Raw.update_accept_ctx accept listen
let update_connect_ctx _v fd = Raw.update_connect_ctx fd

let leaked : (Handle.t * int Overlapped.t * roots) list ref = ref []

let finaliser v =
  H.iter (fun _ (fd, ol, _) -> try Raw.cancel fd ol with Unix.Unix_error _ -> ()) v.in_flight;
  let rec loop fuel =
    if H.length v.in_flight = 0 then ()
    else if fuel = 0 then
      H.iter (fun _ entry -> leaked := entry :: !leaked) v.in_flight
    else match (try wait v ~timeout:100 with Unix.Unix_error _ -> None) with
      | None -> loop (fuel - 1)         (* no completion within the timeout *)
      | Some _ -> loop fuel             (* reaped one; that is progress *)
  in
  loop 5

let create ?(overlapped = 1024) n =
  let v =
    { iocp = Raw.create_io_completion_port n
    ; next_id = 1
    ; in_flight = H.create 255
    ; free = List.init overlapped (fun _ -> Overlapped.create 0) }
  in
  Gc.finalise finaliser v;
  v

(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>
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
