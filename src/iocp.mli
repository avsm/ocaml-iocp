(* Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>
   SPDX-License-Identifier: MIT *)

(** {2 Input/Output Completion Ports} 
    
    Bindings to input/output completion ports (IOCP) allowing for efficient,
    asynchronous IO on Windows. *)

module Overlapped = Overlapped
module Handle = Handle
module Raw = Prototype_raw

type t
type fd = Handle.t

exception Out_of_overlapped

module Id : sig
  type t
  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool
  val to_int : t -> int
end

module H : Hashtbl.S with type key = Id.t

val create : int -> t

val handle_of_fd : t -> Unix.file_descr -> int -> fd

val openfile :
  string -> Unix.open_flag list -> Unix.file_perm -> Unix.file_descr
(** Open a file for overlapped I/O and return a caller-owned descriptor. Accept
    access, append, creation, truncation, exclusive creation and inheritance
    flags. Raise [Invalid_argument] for unsupported flags. Inheritance requires
    [O_KEEPEXEC] without [O_CLOEXEC]. Creation applies the requested Windows
    read-only attribute when opening the file. Append restricts writable handles
    to appending. Read-only handles keep their original access. *)

val pipe :
  ?read_async:bool ->
  ?write_async:bool ->
  unit ->
  Unix.file_descr * Unix.file_descr
(** Create a private, non-inheritable byte pipe and return its read and write
    ends. The caller closes both with [Unix.close]. Both support overlapped I/O
    by default. Set an end's option to false for synchronous child standard I/O.
    Only asynchronous ends may use a port. *)

val read : t -> fd -> Cstruct.buffer -> pos:int -> len:int -> off:Optint.Int63.t -> Id.t

val write : t -> fd -> Cstruct.buffer -> pos:int -> len:int -> off:Optint.Int63.t -> Id.t

type completion_status = {
  bytes_transferred : int;
  id : Id.t;
  error : Unix.error option;
}

val completion_status : t -> timeout:int -> completion_status option
