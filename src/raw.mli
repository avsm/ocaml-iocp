type t
type id = int

val create_io_completion_port : int -> t

external associate_fd_with_iocp : t -> Unix.file_descr -> id -> Handle.t
  = "ocaml_iocp_associate_fd_with_iocp"

external openfile :
  t -> id -> string -> Unix.open_flag list -> Unix.file_perm -> Handle.t
  = "ocaml_iocp_unix_open"

val pipe : t -> id -> id -> string -> Handle.t * Handle.t

type completion_status = private
  | Cs_none
  | Cs_some of {
      handle_id : id;
      bytes_transferred : int;
      overlapped_id : int;
      error : Unix.error option;
    }

external get_queued_completion_status : t -> int -> completion_status
  = "ocaml_iocp_get_queued_completion_status"

type unsafe_completion_status = private {
  mutable handle_id : id;
  mutable bytes_transferred : int;
  mutable overlapped_id : int;
      (* Nb, first 3 fields identical to Some clause of completion_status above*)
  mutable success : bool;
  mutable err : int;
}

external get_queued_completion_status_unsafe :
  t -> int -> unsafe_completion_status -> unit
  = "ocaml_iocp_get_queued_completion_status_unsafe"
(** The zero-allocation, unmapped alternative to {!get_queued_completion_status}:
    it fills a caller-provided record and leaves the raw Win32 code in [err] for
    the caller to map. Used by standalone ocaml-iocp consumers; the eio backend
    uses the allocating, error-mapped variant. *)

val make_unsafe_completion_status : unit -> unsafe_completion_status

(* Operations. The handle is already associated with a completion port, so these
   don't take the port — completions are delivered to the associated port. *)
external read :
  Handle.t -> Cstruct.buffer -> int -> int -> 'a Overlapped.t -> unit
  = "ocaml_iocp_read"

external write :
  Handle.t -> Cstruct.buffer -> int -> int -> 'a Overlapped.t -> unit
  = "ocaml_iocp_write"

external accept :
  Handle.t -> Handle.t -> Cstruct.buffer -> 'a Overlapped.t -> unit
  = "ocaml_iocp_accept"

external connect :
  Handle.t -> Sockaddr.t -> 'a Overlapped.t -> unit
  = "ocaml_iocp_connect"

external send : Handle.t -> Wsabuf.t -> 'a Overlapped.t -> unit = "ocaml_iocp_send"
external recv : Handle.t -> Wsabuf.t -> 'a Overlapped.t -> unit = "ocaml_iocp_recv"
external cancel : Handle.t -> 'a Overlapped.t -> unit = "ocaml_iocp_cancel"
external cancel_all : Handle.t -> unit = "ocaml_iocp_cancel_all"

external recv_from :
  Handle.t -> Wsabuf.t -> Sockaddr.t -> 'a Overlapped.t -> unit
  = "ocaml_iocp_recv_from"

external send_to :
  Handle.t -> Wsabuf.t -> Sockaddr.t -> 'a Overlapped.t -> unit
  = "ocaml_iocp_send_to"

(** [post t ~key ~bytes] posts a key-only completion packet (no OVERLAPPED).
    Used to wake a thread in {!get_queued_completion_status} or to relay a
    {!register_wait} firing. Safe to call from any thread. *)
external post : t -> int -> int -> unit = "ocaml_iocp_post"

(** [register_wait t handle key] posts a packet carrying [key] when [handle]
    (e.g. a process handle) becomes signalled. Returns an opaque token to pass
    to {!unregister_wait}. *)
external register_wait : t -> Unix.file_descr -> int -> nativeint
  = "ocaml_iocp_register_wait"

external unregister_wait : nativeint -> unit = "ocaml_iocp_unregister_wait"

external update_accept_ctx : Handle.t -> Handle.t -> unit = "ocaml_iocp_update_accept_ctx"
external update_connect_ctx : Handle.t -> unit = "ocaml_iocp_update_connect_ctx"
external shutdown : Handle.t -> Unix.shutdown_command -> unit = "ocaml_iocp_shutdown"