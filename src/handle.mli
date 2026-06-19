type t

val fd : t -> Unix.file_descr

val of_fd : Unix.file_descr -> t
(** [of_fd fd] views [fd] as a handle for submitting operations.
    {!Iocp.handle_of_fd}); this is just the inverse of {!fd}. *)
