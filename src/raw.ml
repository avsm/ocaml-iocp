type t
(* A handle to an I/O completion port *)

type id = int
(* An identifier associated with a Handle *)

(* Completion Port Management *)
external create_io_completion_port : int -> t
  = "ocaml_iocp_create_io_completion_port"

(* File descriptors must be associated with an IOCP before we do any IO on them *)
external associate_fd_with_iocp : t -> Unix.file_descr -> id -> Handle.t
  = "ocaml_iocp_associate_fd_with_iocp"

external openfile :
  t -> id -> string -> Unix.open_flag list -> Unix.file_perm -> Handle.t
  = "ocaml_iocp_unix_open"

external pipe' : t -> id -> id -> string -> Handle.t * Handle.t
  = "ocaml_iocp_unix_pipe"

let pipe iocp id1 id2 name =
  let path = "\\\\.\\pipe\\" ^ name in
  pipe' iocp id1 id2 path

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

type unsafe_completion_status = {
  mutable handle_id : id;
  mutable bytes_transferred : int;
  mutable overlapped_id : int;
  mutable success : bool;
  mutable err : int;
}

external get_queued_completion_status_unsafe :
  t -> int -> unsafe_completion_status -> unit
  = "ocaml_iocp_get_queued_completion_status_unsafe"

let make_unsafe_completion_status () =
  {
    handle_id = 0;
    bytes_transferred = 0;
    overlapped_id = 0;
    success = false;
    err = 0;
  }

(* Operations. The handle is already associated with a completion port, so these
   don't take the port — completions are delivered to the associated port. *)
external read :
  Handle.t -> Cstruct.buffer -> int -> int -> 'a Overlapped.t -> unit
  = "ocaml_iocp_read"

external write :
  Handle.t -> Cstruct.buffer -> int -> int -> 'a Overlapped.t -> unit
  = "ocaml_iocp_write"

external accept :
  Handle.t -> Handle.t  -> Cstruct.buffer -> 'a Overlapped.t -> unit
  = "ocaml_iocp_accept"

external connect :
  Handle.t -> Sockaddr.t -> 'a Overlapped.t -> unit
  = "ocaml_iocp_connect"

external send : Handle.t -> Wsabuf.t -> 'a Overlapped.t -> unit
  = "ocaml_iocp_send"

external recv : Handle.t -> Wsabuf.t  -> 'a Overlapped.t -> unit
  = "ocaml_iocp_recv"

external cancel : Handle.t -> 'a Overlapped.t -> unit = "ocaml_iocp_cancel"

(* Datagram operations carrying a peer address. *)
external recv_from :
  Handle.t -> Wsabuf.t -> Sockaddr.t -> 'a Overlapped.t -> unit
  = "ocaml_iocp_recv_from"

external send_to :
  Handle.t -> Wsabuf.t -> Sockaddr.t -> 'a Overlapped.t -> unit
  = "ocaml_iocp_send_to"

(* Post a key-only completion packet (no OVERLAPPED); used for wakeups and to
   deliver the result of a [register_wait]. Safe to call from any thread. *)
external post : t -> int -> int -> unit = "ocaml_iocp_post"

(* Bridge a waitable HANDLE to the port: when [handle] is signalled, a packet
   carrying [key] is posted. Returns an opaque token for [unregister_wait]. *)
external register_wait : t -> Unix.file_descr -> int -> nativeint
  = "ocaml_iocp_register_wait"

external unregister_wait : nativeint -> unit = "ocaml_iocp_unregister_wait"

(* Apply the socket context updates Winsock needs after AcceptEx/ConnectEx so
   the accepted/connected socket is usable with getpeername/shutdown/etc. *)
(* [update_accept_ctx accept ~listen] — the accepted socket and the listening
   socket it came from. *)
external update_accept_ctx : Handle.t -> Handle.t -> unit = "ocaml_iocp_update_accept_ctx"
external update_connect_ctx : Handle.t -> unit = "ocaml_iocp_update_connect_ctx"

external shutdown : Handle.t -> Unix.shutdown_command -> unit = "caml_iocp_shutdown"
