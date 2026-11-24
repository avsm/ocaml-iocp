(* Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>
   SPDX-License-Identifier: MIT *)

(** Windows I/O completion ports.

    A port delivers completions for registered handles and for individual
    requests on borrowed descriptors (see {!Event}). Threads and domains may submit and cancel requests concurrently.
    Several threads may wait on the same port. Each queued packet is delivered
    once.

    {1:delivery Completion delivery}

    Attach an OCaml value to each submission. A queued completion returns this
    value to the consumer. Completions may arrive in any order. The default port
    queues one completion per successful submission. With [skip_on_success] enabled, check
    {!result} on the returned request. [Some c] delivers the completion
    immediately. [None] means the port delivers it through {!wait}, possibly to
    a consumer already running on another domain.

    The port passed to a submission receives its completion. It may differ
    from the port used to register the handle. Pending operations need no consumer
    on the registration port. Cancel a request on its
    delivery port. Shut down a socket on its registration port.

    This example uses a private port with one request and no notifications.
    {[
    let read_at_start port handle buf =
      let req = Iocp.read port ~data:() handle buf ~offset:0L in
      let c =
        match Iocp.result req with
        | Some c -> c
        | None ->
          let rec wait () = match Iocp.wait port ~timeout:(-1) with
            | Some (Iocp.Completion ((), c)) -> c
            | Some Wake | None -> wait ()
          in
          wait ()
      in
      match c.error with
      | None -> c.bytes_transferred
      | Some e -> raise (Unix.Unix_error (e, "read", ""))
    ]}

    {1:lifetime Ownership and shutdown}

    The caller owns the descriptor. A {!handle} borrows it until {!close_handle}
    returns. The library keeps submitted buffers reachable. Keep each submitted
    byte range, including overlapping slices, untouched until its completion is
    delivered. At that point the range may be reused.

    To shut down a port, stop new submissions, request cancellation if needed,
    and drain outstanding completions. Close each registration, then close the
    port. Close the underlying descriptors separately. *)

type 'a t
(** A completion port and its outstanding requests. *)

type 'a handle
(** A borrowed registration of an overlapped handle with a port. *)

type 'a request
(** A submission token for {!cancel} and {!result}. It may be discarded while
    the port is awaiting its completion. *)

type completion = {
  bytes_transferred : int;
      (** Counts the bytes transferred. A failed operation may report a partial
          transfer. *)
  error : Unix.error option;
      (** Is [None] on success or [Some e] on failure. *)
  source : Unix.sockaddr option;
      (** Gives the sender on successful datagram reception. *)
}
(** Describes a completed operation. *)

type 'a event = Wake | Completion of 'a * completion
(** A wake notification or the submitted value and its I/O result. *)

(** {1 Completion ports} *)

val create : ?skip_on_success:bool -> unit -> 'a t
(** Create a port. By default, {!wait} delivers every completion. If
    [skip_on_success] is true, supported handles deliver immediate completions
    through {!result}. Check {!result} after every submission in this mode. *)

val close : 'a t -> unit
(** Close the port. Repeated calls have no effect. Raise [Invalid_argument] if a
    consumer is waiting, requests remain pending or registrations remain open.
*)

val pending : 'a t -> int
(** Return the number of requests still awaiting delivery through {!wait},
    including I/O still in progress. Wake notifications are excluded. *)

val same_handle : Unix.file_descr -> Unix.file_descr -> bool
(** Return whether two open descriptors refer to the same Windows kernel object.
    Both descriptors must remain open during the call. *)

val register : 'a t -> Unix.file_descr -> 'a handle
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

type file_key
(** Identifies an association by physical equality across live aliases. *)

val file_key : 'a handle -> file_key
(** Return the shared key for this registration. Registering the file again
    after the last registration closes creates a fresh key. *)

val close_handle : 'a handle -> unit
(** Close a registration. Repeated calls have no effect. Raise
    [Invalid_argument] while its requests remain pending. Closing the last
    registration detaches the object from the port, allowing later registration
    with another port. The original descriptor remains open and belongs to the
    caller. If native detachment fails, [Unix.Unix_error] is raised and the
    registration is still closed. The object may remain associated with the
    old port until its descriptors are closed. *)

val wake : 'a t -> unit
(** Queue a [Wake] notification. This function may be called from another thread or domain. *)

val wait : 'a t -> timeout:int -> 'a event option
(** Consume the next queued completion, or return [None] on timeout or
    interruption. [timeout] is in milliseconds. Zero polls and [-1] waits
    indefinitely. Raise [Invalid_argument] for other negative values. Finite
    timeouts must not exceed [0xffff_fffe]. If constructing a completion raises
    [Out_of_memory], another call to [wait] can collect the packet. *)

val result : 'a request -> completion option
(** Return an immediate completion, if one was produced during submission.
    Otherwise return [None], even if {!wait} has since consumed the completion.
    Immediate completions are not queued and do not count towards {!pending}. If
    constructing the result raises [Out_of_memory], call [result] again. *)

val cancel : 'a t -> 'a request -> unit
(** Request cancellation. Cancellation may race with successful completion. A
    pending request still requires a delivery through {!wait}. Cancelling a
    completed request has no effect. Raise [Invalid_argument] if the request
    belongs to another port. *)

val cancel_all : 'a t -> unit
(** Request cancellation of all pending operations. Drain them with {!wait}
    before closing the port. *)

(** {1 I/O operations}

    Pass the port that should deliver the result. A closed port or registration
    raises [Invalid_argument]. A Windows error during submission may raise
    [Unix.Unix_error] directly. In that case there is no completion to drain.
    For a returned request, inspect the [error] field on delivery. Winsock may
    cancel pending socket I/O when the submitting OS thread exits. *)

val read : 'a t -> data:'a -> 'a handle -> Cstruct.t -> offset:int64 -> 'a request
(** Read into the buffer at the given file offset. Use a non-negative offset for
    files and [0L] for pipes. End of input completes successfully with zero
    bytes. *)

val write : 'a t -> data:'a -> 'a handle -> Cstruct.t -> offset:int64 -> 'a request
(** Write from the buffer at the given file offset. Use a non-negative offset
    for files, [-1L] for atomic append, and [0L] for pipes. *)

val recv : 'a t -> data:'a -> 'a handle -> Cstruct.t -> 'a request
(** Receive stream data into the buffer. End of input completes successfully
    with zero bytes. *)

val send : 'a t -> data:'a -> 'a handle -> Cstruct.t -> 'a request
(** Send stream data from the buffer. The completion reports the number of bytes
    sent, which may be less than the buffer length. *)

val recvfrom : 'a t -> data:'a -> 'a handle -> Cstruct.t -> 'a request
(** Receive a datagram into the buffer. The completion includes its source
    address. A zero-length datagram preserves its source address. An oversized
    datagram reports [EMSGSIZE] with the copied byte count and source address. *)

val sendto : 'a t -> data:'a -> 'a handle -> Cstruct.t -> Unix.sockaddr -> 'a request
(** Send the buffer as one datagram to the given address. *)

val accept : 'a t -> data:'a -> 'a handle -> client:Unix.file_descr -> 'a request
(** Accept a connection into [client], an unbound, unconnected socket of the
    listener's family and type. The caller supplies and owns [client]. Reserve
    it exclusively for this accept until completion. On success,
    [Unix.getpeername client] returns the peer and {!register} can register
    [client] for further I/O. *)

val connect : 'a t -> data:'a -> 'a handle -> Unix.sockaddr -> 'a request
(** Connect a bound socket to the given address. Successful completion updates
    its connection context. *)

val shutdown : 'a t -> 'a handle -> Unix.shutdown_command -> unit
(** Shut down a socket on its registration port. Receive shutdown cancels
    outstanding registered receives on every delivery port, including receives
    through duplicated descriptors. For event-based receives, call
    {!Event.cancel_receives} on each submitting port after shutdown. An aborted
    receive then completes successfully with zero bytes and no source address. A
    receive that already completed keeps its data or error. If a duplicated
    socket lacks its connection context, shutdown tries its other live
    registrations for the same kernel object. *)

(** Per-request event delivery. These operations have the same arguments and
    results as the registered operations, but borrow an overlapped descriptor
    directly. Keep it open and unchanged until delivery, including after
    cancellation. Its completion-port association and notification modes are
    preserved. A pending operation signals a private event that delivers its
    completion to the submitting port. If routing fails, the caller drains the
    operation before returning or raising. Other threads can still use the port. *)
module Event : sig
  val read : 'a t -> data:'a -> Unix.file_descr -> Cstruct.t -> offset:int64 -> 'a request

  val write : 'a t -> data:'a -> Unix.file_descr -> Cstruct.t -> offset:int64 -> 'a request

  val recv : 'a t -> data:'a -> Unix.file_descr -> Cstruct.t -> 'a request

  val send : 'a t -> data:'a -> Unix.file_descr -> Cstruct.t -> 'a request

  val recvfrom : 'a t -> data:'a -> Unix.file_descr -> Cstruct.t -> 'a request

  val sendto : 'a t -> data:'a -> Unix.file_descr -> Cstruct.t -> Unix.sockaddr -> 'a request

  val accept : 'a t -> data:'a -> Unix.file_descr -> client:Unix.file_descr -> 'a request

  val connect : 'a t -> data:'a -> Unix.file_descr -> Unix.sockaddr -> 'a request

  val cancel_receives : 'a t -> Unix.file_descr -> unit
  (** After shutting down receive on a socket, call this on each port that
      submitted event-based receives on it. This also covers duplicates.
      Aborted receives complete as end of input. Completed transfers and
      errors keep their results. A closed port has no pending receives, so this
      call has no effect. *)
end

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

module Interrupt : sig
  type t

  val current : unit -> t
  (** Capture the calling OS thread as a wakeup target. *)

  val signal : t -> unit
  (** Interrupt an alertable wait on the captured thread. A signal sent before
      an IOCP wait also wakes the next call to {!wait} on that thread. *)

  val close : t -> unit
  (** Release the target. Later signals have no effect. *)
end
