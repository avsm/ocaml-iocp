(* Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>
   SPDX-License-Identifier: MIT *)

(** Native IOCP bindings. The public module supplies locking, request IDs and
    lifetime checks. Keep descriptors open until their completions are consumed.
    Submitted operations root their buffers until completion. *)

type port

external create_port : unit -> port = "iocp_create"
external close_port : port -> unit = "iocp_close"
external associate_port : port -> Unix.file_descr -> unit = "iocp_associate"
external detach : Unix.file_descr -> unit = "iocp_detach"
external wake_port : port -> unit = "iocp_wake"

type completion = {
  id : int;
  bytes_transferred : int;
  error : Unix.error option;
}

external dequeue : port -> int -> int = "iocp_dequeue"
(** Return the oldest packet's ID, or [-1] on timeout. Repeat that ID until
    [acknowledge] commits delivery. Both functions require the consumer lock. *)

external acknowledge : port -> unit = "iocp_acknowledge" [@@noalloc]

external same_handle : Unix.file_descr -> Unix.file_descr -> bool
  = "iocp_same_handle"

type access = Read | Write | Read_write

external reopen : Unix.file_descr -> access -> append:bool -> share_delete:bool -> Unix.file_descr
  = "iocp_reopen"

type disposition = Create_new | Create_always | Open_always

external create_readonly :
  string -> access -> append:bool -> share_delete:bool -> disposition -> Unix.file_descr
  = "iocp_create_readonly"
(** A later reopen for write access would be denied by the read-only attribute.
*)

external pipe_raw : bool -> bool -> message:bool -> Unix.file_descr * Unix.file_descr
  = "iocp_pipe"
