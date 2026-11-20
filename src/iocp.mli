(* Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>
   SPDX-License-Identifier: MIT *)

(** Windows I/O completion ports.

    A port delivers completions for operations on registered files, pipes and
    sockets. Threads and domains may submit and cancel requests concurrently.
    Each port admits one caller of {!wait} at a time.

    {1:delivery Completion delivery}

    Give each outstanding operation a positive ID and use the ID to dispatch its
    completion. Completions may arrive in any order. The default port queues one
    completion per successful submission. With [skip_on_success] enabled, check
    {!result} on the returned request. [Some c] delivers the completion
    immediately. [None] means the port delivers it through {!wait}, possibly to
    a consumer already running on another domain. Reserve the ID and publish any
    dispatch state before submitting.

    This example uses a private port with one request and no notifications.
    {[
    let read_at_start port handle buf =
      let req = Iocp.read port ~id:1 handle buf ~offset:0L in
      let c =
        match Iocp.result req with
        | Some c -> c
        | None -> Option.get (Iocp.wait port ~timeout:(-1))
      in
      match c.error with
      | None -> c.bytes_transferred
      | Some e -> raise (Unix.Unix_error (e, "read", ""))
    ]}

    {1:lifetime Ownership and shutdown}

    The caller owns the descriptor. A {!handle} borrows it until {!close_handle}
    returns. The library keeps submitted buffers reachable. Keep each submitted
    byte range, including overlapping slices, untouched until its completion is
    delivered. At that point the range and ID may be reused.

    To shut down a port, stop new submissions, request cancellation if needed,
    and drain outstanding completions. Close each registration, then close the
    port. Close the underlying descriptors separately. *)

type t
(** A completion port and its outstanding requests. *)

type handle
(** A borrowed registration of an overlapped handle with a port. *)

type request
(** A submission token for {!cancel} and {!result}. It may be discarded while
    the port is awaiting its completion. *)

type completion = {
  id : int;  (** Identifies the request, or is zero for a notification. *)
  bytes_transferred : int;
      (** Counts the bytes transferred. A failed operation may report a partial
          transfer. *)
  error : Unix.error option;
      (** Is [None] on success or [Some e] on failure. *)
  source : Unix.sockaddr option;
      (** Gives the sender on successful datagram reception. *)
}
(** Describes a completed operation. *)

(** {1 Completion ports} *)

val create : ?skip_on_success:bool -> unit -> t
(** Create a port. By default, {!wait} delivers every completion. If
    [skip_on_success] is true, supported handles deliver immediate completions
    through {!result}. Check {!result} after every submission in this mode. *)

val close : t -> unit
(** Close the port. Repeated calls have no effect. Raise [Invalid_argument] if a
    consumer is waiting, requests remain pending or registrations remain open.
*)

val pending : t -> int
(** Return the number of requests still awaiting delivery through {!wait},
    including I/O still in progress. Wake notifications are excluded. *)

val register : t -> Unix.file_descr -> handle
(** [register port fd] borrows an overlapped descriptor. Keep [fd] open and
    unchanged, including across [Unix.dup2], until this registration is closed.
    Each successful call requires its own {!close_handle}. Duplicated
    descriptors for the same kernel object may share a port. Registration on a
    different port raises [Unix.Unix_error] with [EINVAL] while it is
    associated.

    Start with no outstanding external I/O. While registered, submit overlapped
    operations exclusively through this library, including through aliases. The
    first submission can enable Windows' skip-on-success notification mode even
    for a port using queued delivery. Windows retains this mode after detach. A
    subsequent native consumer must handle synchronous success itself. *)

val close_handle : handle -> unit
(** Close a registration. Repeated calls have no effect. Raise
    [Invalid_argument] while its requests remain pending. Closing the last
    registration detaches the object from the port, allowing later registration
    with another port. The original descriptor remains open and belongs to the
    caller. *)

val wake : t -> unit
(** Queue a notification with [id = 0], zero bytes and no error or source
    address. This function may be called from another thread or domain. *)

val wait : t -> timeout:int -> completion option
(** Consume the next queued completion, or return [None] on timeout. [timeout]
    is in milliseconds. Zero polls and [-1] waits indefinitely. Raise
    [Invalid_argument] for concurrent consumers or other negative values. Finite
    timeouts must not exceed [0xffff_fffe]. Sequential calls may use different
    OS threads. If constructing the completion raises [Out_of_memory], retry
    [wait] to collect the same packet. *)

val result : request -> completion option
(** Return an immediate completion, if one was produced during submission.
    Otherwise return [None], even if {!wait} has since consumed the completion.
    Immediate completions are not queued and do not count towards {!pending}. If
    constructing the result raises [Out_of_memory], call [result] again. *)

val cancel : t -> request -> unit
(** Request cancellation. Cancellation may race with successful completion. A
    pending request still requires a delivery through {!wait}. Cancelling a
    completed request has no effect. Raise [Invalid_argument] if the request
    belongs to another port. *)

val cancel_all : t -> unit
(** Request cancellation of all pending operations. Drain them with {!wait}
    before closing the port. *)

(** {1 I/O operations}

    Use the port that owns the registration. A closed registration, the wrong
    port, a non-positive ID or an outstanding duplicate ID raises
    [Invalid_argument]. A Windows error during submission may raise
    [Unix.Unix_error] directly. In that case there is no completion to drain.
    For a returned request, inspect the [error] field on delivery. Winsock may
    cancel pending socket I/O when the submitting OS thread exits. *)

val read : t -> id:int -> handle -> Cstruct.t -> offset:int64 -> request
(** Read into the buffer at the given file offset. Use a non-negative offset for
    files and [0L] for pipes. End of input completes successfully with zero
    bytes. *)

val write : t -> id:int -> handle -> Cstruct.t -> offset:int64 -> request
(** Write from the buffer at the given file offset. Use a non-negative offset
    for files, [-1L] for atomic append, and [0L] for pipes. *)

val recv : t -> id:int -> handle -> Cstruct.t -> request
(** Receive stream data into the buffer. End of input completes successfully
    with zero bytes. *)

val send : t -> id:int -> handle -> Cstruct.t -> request
(** Send stream data from the buffer. The completion reports the number of bytes
    sent, which may be less than the buffer length. *)

val recvfrom : t -> id:int -> handle -> Cstruct.t -> request
(** Receive a datagram into the buffer. The completion includes its source
    address. A zero-length datagram preserves its source address. An oversized
    datagram reports [EMSGSIZE] with the copied byte count and source address. *)

val sendto : t -> id:int -> handle -> Cstruct.t -> Unix.sockaddr -> request
(** Send the buffer as one datagram to the given address. *)

val accept : t -> id:int -> handle -> client:Unix.file_descr -> request
(** Accept a connection into [client], an unbound, unconnected socket of the
    listener's family and type. The caller supplies and owns [client]. Reserve
    it exclusively for this accept until completion. On success,
    [Unix.getpeername client] returns the peer and {!register} can register
    [client] for further I/O. *)

val connect : t -> id:int -> handle -> Unix.sockaddr -> request
(** Connect a bound socket to the given address. Successful completion updates
    its connection context. *)

val shutdown : t -> handle -> Unix.shutdown_command -> unit
(** Shut down a socket. Receive shutdown also cancels outstanding receives on
    this port, including receives through duplicated descriptors. An aborted
    receive then completes successfully with zero bytes and no source address. A
    receive that already completed keeps its data or error. If a duplicated
    socket lacks its connection context, shutdown tries its other live
    registrations for the same kernel object. *)

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

module Wakeup : sig
  (** Counted scheduler notifications for threads and domains. *)

  type t
  (** A notification port. *)

  val create : unit -> t
  (** Create an empty notification port. *)

  val signal : t -> unit
  (** Queue one notification without taking an OCaml lock. Signals accumulate
      even when no thread is waiting. *)

  val wait : t -> int -> unit
  (** Consume one notification, or return on timeout or close. The timeout is in
      milliseconds. Zero polls and [-1] waits indefinitely. Other values must
      lie between zero and [0xffff_fffe]. *)

  val close : t -> unit
  (** Wake all waiters and release the port once outstanding calls return.
      Repeated closes, signals and waits on a closed port have no effect.
      Timeout validation also applies to a closed port. *)
end
