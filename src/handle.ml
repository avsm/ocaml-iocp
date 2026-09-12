type t = Unix.file_descr

let fd : t -> Unix.file_descr = fun x -> x
let of_fd : Unix.file_descr -> t = fun x -> x
