type t

val fd : t -> Unix.file_descr

val of_fd : Unix.file_descr -> t
(** [of_fd fd] views an already-overlapped, port-associated [fd] as a handle for
    submitting operations. It is the inverse of {!fd} and performs no syscall;
    association with a port is done separately by {!Iocp.handle_of_fd}. *)
