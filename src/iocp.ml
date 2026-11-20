(*----------------------------------------------------------------------
   Copyright (c) 2022 Patrick Ferris <patrick@sirref.org>
   Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>
   Distributed under the MIT license. See terms at the end of this file.
  ----------------------------------------------------------------------*)

module Handle = Handle
module Overlapped = Overlapped
module Native = Raw
module Raw = Prototype_raw

module Id = struct
  type t = int
  let compare : t -> t -> int = Int.compare
  let hash : t -> int = Hashtbl.hash
  let equal : t -> t -> bool = Int.equal
  let to_int (a:int) = a
end

type fd = Handle.t

module H = Hashtbl.Make(Id)

type roots =
  | ReadWrite of Cstruct.buffer

type t = {
  iocp : Raw.t;
  m : Mutex.t;
  mutable external_key : int;
  in_flight : (Handle.t * int Overlapped.t * roots * int) H.t;
  mutable unused_overlapped : int Overlapped.t list;
  allocated_overlapped : int Overlapped.t array;
  external_keys : int array;
}

type completion_status = {
  bytes_transferred : int;
  id : Id.t;
  error : Unix.error option;
}

exception Out_of_overlapped

let create ?(overlapped = 1024) n =
  let allocated_overlapped = Array.init overlapped (fun i -> Overlapped.create i) in
  let external_keys = Array.init overlapped (fun _ -> -1) in
  let unused_overlapped = Array.to_list allocated_overlapped in
  (* Format.eprintf "All created\n%!"; *)
  { iocp = Raw.create_io_completion_port n
  ; m = Mutex.create ()
  ; in_flight = H.create 255
  ; external_key = 1
  ; unused_overlapped
  ; allocated_overlapped
  ; external_keys }

let handle_of_fd v fd key =
  Raw.associate_fd_with_iocp v.iocp fd key

let get_overlapped v fd root =
  Mutex.lock v.m;
  match v.unused_overlapped with
  | [] -> Mutex.unlock v.m; raise Out_of_overlapped
  | ol :: ols ->
    (* Format.eprintf "Getting overlapped\n%!"; *)
    let id = Overlapped.id ol in
    let external_key = v.external_key in
    let key = Overlapped.get_key ol in
    v.external_key <- external_key + 1;
    v.external_keys.(key) <- external_key;
    v.unused_overlapped <- ols;
    H.replace v.in_flight id (fd, ol, root, external_key);
    Mutex.unlock v.m;
    (* Format.eprintf "Found an overlapped: id=%d external_key=%d internal_key=%d\n%!" id external_key key; *)
    ol, external_key

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
    if has Unix.O_RDWR then (Unix.O_RDWR, Native.Read_write)
    else if has O_WRONLY then (Unix.O_WRONLY, Native.Write)
    else (Unix.O_RDONLY, Native.Read)
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
    if access <> Native.Read && has O_CREAT && perm land 0o200 = 0 then
      Native.create_readonly path access (has O_APPEND) ~exclusive:(has O_EXCL)
        ~truncate:(has O_TRUNC)
    else
      let original = Unix.openfile path open_flags perm in
      Fun.protect
        ~finally:(fun () -> Unix.close original)
        (fun () -> Native.reopen original access (has O_APPEND))
  in
  try
    if has O_KEEPEXEC && not (has O_CLOEXEC) then Unix.clear_close_on_exec fd;
    fd
  with ex ->
    Unix.close fd;
    raise ex

let read : t -> Handle.t -> Cstruct.buffer -> pos:int -> len:int -> off:Optint.Int63.t -> Id.t =
  fun v fd buf ~pos ~len ~off ->
    let root = ReadWrite buf in
    let ol, external_key = get_overlapped v fd root in
    Overlapped.set_offset ol off;
    Raw.read v.iocp fd buf len pos ol;
    external_key

let write : t -> Handle.t -> Cstruct.buffer -> pos:int -> len:int -> off:Optint.Int63.t -> Id.t =
  fun v fd buf ~pos ~len ~off -> 
    let root = ReadWrite buf in
    let ol, external_key = get_overlapped v fd root in
    Overlapped.set_offset ol off;
    Raw.write v.iocp fd buf len pos ol;
    external_key

let completion_status : t -> timeout:int -> completion_status option =
  fun v ~timeout ->
    match Raw.get_queued_completion_status v.iocp timeout with
    | Raw.Cs_none -> None
    | Raw.Cs_some cs ->
      Mutex.lock v.m;
      (* let (_buf, _ol) = H.find v.in_flight cs.overlapped_id in *)
      H.remove v.in_flight cs.overlapped_id;
      let ol_key = Overlapped.unsafe_key cs.overlapped_id in
      let external_key = v.external_keys.(ol_key) in
      v.unused_overlapped <- v.allocated_overlapped.(ol_key) :: v.unused_overlapped;
      Mutex.unlock v.m;
      Some { bytes_transferred = cs.bytes_transferred
          ; id = external_key
          ; error = cs.error }

let garbage = Atomic.make []

let finaliser v =
  (* Format.eprintf "Finaliser for Safest.t\n%!"; *)
  H.iter (fun _ (fd, ol, _, _) ->
    Format.eprintf "Cancelling somthing\n%!";
    Raw.cancel fd ol) v.in_flight;
  let rec loop () =
    if (H.length v.in_flight = 0) then ( (*Format.eprintf "All done\n%!" *) ()) else begin
      Format.eprintf "Something to wait for\n%!";
      match completion_status v ~timeout:100 with
      | None ->
        Format.eprintf "Error, failed to cancel some outstanding operations (%d)" (H.length v.in_flight);
        let rec add_to_garbage () =
          let cur = Atomic.get garbage in
          if Atomic.compare_and_set garbage cur (v :: cur)
          then ()
          else add_to_garbage ()
        in
        add_to_garbage ()
      | _ -> loop ()
      end
    in loop ()

let create n =
  let v = create n in
  Gc.finalise finaliser v;
  v

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
