type t
type id = int

val create_io_completion_port : int -> t

external associate_fd_with_iocp : t -> Unix.file_descr -> id -> Handle.t
  = "ocaml_iocp_associate_fd_with_iocp"

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

external peek : t -> completion_status = "ocaml_iocp_peek"

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

val make_unsafe_completion_status : unit -> unsafe_completion_status

(* Operations *)
external read :
  t -> Handle.t -> Cstruct.buffer -> int -> int -> 'a Overlapped.t -> unit
  = "ocaml_iocp_read_bytes" "ocaml_iocp_read"

external write :
  t -> Handle.t -> Cstruct.buffer -> int -> int -> 'a Overlapped.t -> unit
  = "ocaml_iocp_write_bytes" "ocaml_iocp_write"

external cancel : Handle.t -> 'a Overlapped.t -> unit = "ocaml_iocp_cancel"

type access = Read | Write | Read_write

external reopen : Unix.file_descr -> access -> append:bool -> share_delete:bool -> Unix.file_descr
  = "iocp_reopen"

type disposition = Create_new | Create_always | Open_always

external create_readonly :
  string -> access -> append:bool -> share_delete:bool -> disposition -> Unix.file_descr
  = "iocp_create_readonly"
(** A later reopen for write access would be denied by the read-only attribute.
*)
