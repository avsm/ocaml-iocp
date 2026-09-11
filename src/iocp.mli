(** {2 Input/Output Completion Ports}

    Bindings to input/output completion ports (IOCP) allowing for efficient,
    asynchronous IO on Windows. *)

module Wsabuf = Wsabuf
module Overlapped = Overlapped
module Handle = Handle
module Raw = Raw
module Sockaddr = Sockaddr

type t
type fd = Handle.t

module Id : sig
  type t
  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool
  val to_int : t -> int
end

module H : Hashtbl.S with type key = Id.t

val create : ?overlapped:int -> int -> t
(** [create ?overlapped n] creates a completion port sized for [n] concurrent
    waiting threads. [overlapped] (default 1024) bounds the number of operations
    that may be in flight at once; submitting more returns [None] (see below). *)

val close : t -> unit
(** [close t] cancels everything still in flight, reaps the aborted completions
    and closes the port. Idempotent, and also run from a finaliser, but worth
    calling explicitly: the port's handle is held until it is, so a program that
    creates ports over its lifetime otherwise accumulates one handle per port. *)

val handle_of_fd : t -> Unix.file_descr -> int -> fd
(** Associate an already-overlapped OS handle with the port under completion
    key [key]. *)

val openfile: t -> int -> string -> Unix.open_flag list -> int -> fd

(** {3 Submitting operations}

    Each submission draws an OVERLAPPED from the fixed pool and returns its
    {!Id.t}, or [None] if the pool is exhausted. The caller should then wait
    for outstanding operations to complete and retry, rather than treating it as
    an error. *)

val read : t -> fd -> Cstruct.buffer -> pos:int -> len:int -> off:Optint.Int63.t -> Id.t option

val write : t -> fd -> Cstruct.buffer -> pos:int -> len:int -> off:Optint.Int63.t -> Id.t option

val accept : t -> fd -> Handle.t -> Cstruct.t -> Id.t option
(** [accept t listen accept_sock buf] starts an overlapped accept. [buf] is a
    scratch buffer from {!Sockaddr.accept_buffer}; recover the peer afterwards
    with {!Sockaddr.of_accept_buffer}. *)

val recv : t -> fd -> Cstruct.t list -> Id.t option

val send : t -> fd -> Cstruct.t list -> Id.t option

val recv_from : t -> fd -> Cstruct.t list -> Sockaddr.t -> Id.t option
(** [recv_from t fd bufs addr] receives a datagram, recording the source address
    in [addr]. [addr] must remain live until the operation completes; read it
    with {!Sockaddr.get} afterwards. *)

val send_to : t -> fd -> Cstruct.t list -> Sockaddr.t -> Id.t option
(** [send_to t fd bufs addr] sends a datagram to [addr]. *)

val connect : t -> fd -> Sockaddr.t -> Id.t option

val cancel : t -> Id.t -> unit
(** [cancel t id] requests cancellation of the operation [id] (via [CancelIoEx]).
    The operation still produces a completion which must be reaped with {!completion_status}.
    Cancelling an unknown/already-completed id is a no-op. *)

val cancel_all : t -> fd -> unit
(** [cancel_all t fd] cancels every outstanding overlapped operation on [fd] (via
    [CancelIoEx] with a NULL overlapped). Each cancelled operation still produces
    an [ERROR_OPERATION_ABORTED] completion to be reaped with {!wait}. Useful to
    unblock a pending recv when shutting down a socket's receive side. *)

val active_ops : t -> int
(** Number of operations submitted but not yet reaped (including cancelled ones
    whose aborted completion has not yet been drained). *)

(** {3 Reaping completions} *)

type completion_status = {
  bytes_transferred : int;
  id : Id.t;
  error : Unix.error option;
}

type packet =
  | Io of completion_status
      (** A real overlapped I/O completion. *)
  | Posted of { key : int; bytes_transferred : int }
      (** A key-only packet from {!post}/{!wakeup} or a fired {!register_wait}. *)

val wait : t -> timeout:int -> packet option
(** [wait t ~timeout] dequeues one completion packet, waiting up to [timeout]
    milliseconds ([-1] waits indefinitely), or returns [None] on timeout. *)

(** {3 Wakeups and waitable handles} *)

val post : t -> key:int -> bytes:int -> unit
(** Post a key-only completion packet. Surfaces as [Posted {key; bytes}]. Safe
    to call from any thread. *)

val wakeup : t -> unit
(** [wakeup t] is [post t ~key:0 ~bytes:0] — the conventional "just wake up". *)

type wait
(** A registered wait, returned by {!register_wait}. *)

val register_wait : t -> Unix.file_descr -> key:int -> wait
(** [register_wait t handle ~key] arranges for a [Posted] packet carrying [key]
    to be delivered when [handle] (e.g. a process handle, which cannot be
    associated with a port) becomes signalled. *)

val unregister_wait : wait -> unit
(** Cancel a {!register_wait} and free its resources (blocking until any
    in-progress callback completes). Idempotent. *)

(** {3 Socket helpers} *)

val shutdown : t -> fd -> Unix.shutdown_command -> unit

val update_accept_ctx : t -> listen:fd -> fd -> unit
(** [update_accept_ctx t ~listen accepted] applies [SO_UPDATE_ACCEPT_CONTEXT] to
    an accepted socket (call after the AcceptEx completion) so it is usable with
    [getpeername]/[shutdown]/etc. [listen] is the listening socket it came from. *)

val update_connect_ctx : t -> fd -> unit
(** Apply [SO_UPDATE_CONNECT_CONTEXT] to a connected socket (call after the
    ConnectEx completion). *)
