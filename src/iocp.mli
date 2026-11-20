(* Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>
   SPDX-License-Identifier: MIT *)

(** Windows completion ports and borrowed handle registrations. *)

type t
(** A completion port. *)

type handle
(** A borrowed registration of an overlapped handle with a port. *)

type completion = {
  id : int;  (** Identifies the request, or is zero for a notification. *)
  bytes_transferred : int;
      (** Counts the bytes transferred. A failed operation may report a partial
          transfer. *)
  error : Unix.error option;  (** Is [None] on success or [Some e] on failure. *)
}
(** Describes a completed operation. *)

(** {1 Completion ports} *)

val create : unit -> t
(** Create a port. {!wait} delivers every completion. *)

val close : t -> unit
(** Close the port. Repeated calls have no effect. Raise [Invalid_argument] if a
    consumer is waiting or registrations remain open. *)

val register : t -> Unix.file_descr -> handle
(** [register port fd] borrows an overlapped descriptor. Keep [fd] open and
    unchanged, including across [Unix.dup2], until this registration is closed.
    Each successful call requires its own {!close_handle}. Duplicated
    descriptors for the same kernel object may share a port. Registration on a
    different port raises [Unix.Unix_error] with [EINVAL] while it is
    associated.

    Start with no outstanding external I/O. While registered, submit overlapped
    operations exclusively through this library, including through aliases. *)

val close_handle : handle -> unit
(** Close a registration. Repeated calls have no effect. Closing the last
    registration detaches the object from the port, allowing later registration
    with another port. The original descriptor remains open and belongs to the
    caller. *)

val wake : t -> unit
(** Queue a notification with [id = 0], zero bytes and no error. This function
    may be called from another thread or domain. *)

val wait : t -> timeout:int -> completion option
(** Consume the next queued completion, or return [None] on timeout. [timeout]
    is in milliseconds. Zero polls and [-1] waits indefinitely. Raise
    [Invalid_argument] for concurrent consumers or other negative values. Finite
    timeouts must not exceed [0xffff_fffe]. Sequential calls may use different
    OS threads. If constructing the completion raises [Out_of_memory], retry
    [wait] to collect the same packet. *)

(** {1 Descriptor constructors} *)

val openfile :
  string -> Unix.open_flag list -> Unix.file_perm -> Unix.file_descr
(** Open a file for overlapped I/O and return a caller-owned descriptor. Accept
    access, append, creation, truncation, exclusive creation and inheritance
    flags. [O_SHARE_DELETE] allows rename or removal while the file is open.
    Raise [Invalid_argument] for unsupported flags. Inheritance requires
    [O_KEEPEXEC] without [O_CLOEXEC]. Creation applies the requested Windows
    read-only attribute when opening the file. Append restricts writable handles
    to appending. Read-only handles keep their original access. *)

val pipe :
  ?read_async:bool ->
  ?write_async:bool ->
  ?message:bool ->
  unit ->
  Unix.file_descr * Unix.file_descr
(** Create a private, non-inheritable pipe and return its read and write
    ends. The caller closes both with [Unix.close]. Both support overlapped I/O
    by default. Set an end's option to false for synchronous child standard I/O.
    Only asynchronous ends may use a port. The default pipe is a byte stream.
    With [message:true], each write is a message and reads retain message
    boundaries. *)
