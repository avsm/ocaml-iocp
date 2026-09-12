(*----------------------------------------------------------------------
   Copyright (c) 2022 Patrick Ferris <patrick@sirref.org>
   Distributed under the MIT license. See terms at the end of this file.
  ----------------------------------------------------------------------*)

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
  | RecvSend of Cstruct.t list
  | RecvSendAddr of Cstruct.t list * Sockaddr.t
  | Connect of Sockaddr.t

(* A [t] is owned by a single domain: submission, reaping and cancellation must
   all happen on that domain. *)
type t = {
  iocp : Raw.t;
  mutable next_id : int;
  in_flight : (Handle.t * int Overlapped.t * roots) H.t;
  mutable free : int Overlapped.t list;
  mutable closed : bool;

  (* Packets dequeued but not yet handed out, four ints each. *)
  batch : int array;
  mutable batch_len : int;
  mutable batch_pos : int;
}

(* Asking for more makes the single-ready-packet case measurably slower without
   helping the busy case any further: at 64 it lost about 15% there and gained
   nothing here. *)
let batch_entries = 8

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

(* Take a slot from the pool and start an operation on it. If [start] raises the
   operation never went pending, so no completion will arrive to release the slot
   and it has to go back here. *)
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

(* Turn the [i]th dequeued packet into a result, recycling its slot. *)
let packet_of_batch v i =
  let key = v.batch.(i * 4)
  and bytes_transferred = v.batch.((i * 4) + 1)
  and overlapped_id = v.batch.((i * 4) + 2)
  and err = v.batch.((i * 4) + 3) in
  if overlapped_id = 0 then
    (* No OVERLAPPED: a packet posted by [post]/[wakeup] or a registered wait. *)
    Posted { key; bytes_transferred }
  else begin
    let id = Overlapped.unsafe_key overlapped_id in
    (match H.find_opt v.in_flight id with
     | Some (_, ol, _) -> H.remove v.in_flight id; v.free <- ol :: v.free
     | None -> ());
    let error = if err = 0 then None else Some (Raw.unix_error_of_win32 err) in
    Io { bytes_transferred; id; error }
  end

let wait v ~timeout =
  if v.batch_pos >= v.batch_len then begin
    v.batch_pos <- 0;
    v.batch_len <- Raw.get_queued_completion_status_ex v.iocp timeout v.batch
  end;
  if v.batch_len = 0 then None
  else begin
    let p = packet_of_batch v v.batch_pos in
    v.batch_pos <- v.batch_pos + 1;
    Some p
  end

let active_ops v = H.length v.in_flight

let cancel v id =
  match H.find_opt v.in_flight id with
  | Some (fd, ol, _) -> Raw.cancel fd ol
  | None -> ()

let cancel_all _v fd = Raw.cancel_all fd

let accept v sock sock_accept addr_buf =
  let buf = Cstruct.to_bigarray addr_buf in
  submit v sock (ReadWrite buf) (fun ol -> Raw.accept sock sock_accept buf ol)

let recv v sock bufs = submit v sock (RecvSend bufs) (fun ol -> Raw.recv sock bufs ol)

let send v sock bufs = submit v sock (RecvSend bufs) (fun ol -> Raw.send sock bufs ol)

(* [addr] is filled in with the source on completion, so it must stay alive. *)
let recv_from v sock bufs addr =
  submit v sock (RecvSendAddr (bufs, addr)) (fun ol -> Raw.recv_from sock bufs addr ol)

let send_to v sock bufs addr =
  submit v sock (RecvSendAddr (bufs, addr)) (fun ol -> Raw.send_to sock bufs addr ol)

let connect v sock addr =
  submit v sock (Connect addr) (fun ol -> Raw.connect sock addr ol)

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
    ; closed = false
    ; batch = Array.make (batch_entries * 4) 0
    ; batch_len = 0
    ; batch_pos = 0 }
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
