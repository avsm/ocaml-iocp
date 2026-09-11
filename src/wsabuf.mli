(** An array of WSABUF descriptors for scatter/gather socket I/O *)

type t

val create : Cstruct.t list -> t
