(* Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>
   SPDX-License-Identifier: MIT *)

(** Native IOCP bindings. The public module supplies locking, request IDs and
    lifetime checks. Keep descriptors open until their completions are consumed.
    Submitted operations root their buffers until completion. *)

type port
type operation

external create_port : unit -> port = "iocp_create"
external close_port : port -> unit = "iocp_close"
external associate_port : port -> Unix.file_descr -> unit = "iocp_associate"
external detach : Unix.file_descr -> unit = "iocp_detach"
external enable_immediate : Unix.file_descr -> bool = "iocp_enable_immediate"
external wake_port : port -> unit = "iocp_wake"

type completion = {
  id : int;
  bytes_transferred : int;
  error : Unix.error option;
  source : Unix.sockaddr option;
}

external dequeue : port -> int -> int = "iocp_dequeue"
(** Return the oldest packet's ID, or [-1] on timeout. Repeat that ID until
    [acknowledge] commits delivery. Both functions require the consumer lock. *)

external acknowledge : port -> unit = "iocp_acknowledge" [@@noalloc]
external completion : operation -> bool -> completion option = "iocp_completion"

type delivery = Native_queue | Post_queue | Immediate

external allocate :
  port -> int -> Unix.file_descr -> 'a -> delivery -> operation
  = "iocp_allocate"

type buffer_op =
  | Read_file
  | Write_file
  | Recv
  | Send
  | Recv_from
  | Send_to of Unix.sockaddr

external submit_buffer : operation -> buffer_op -> Cstruct.t -> int64 -> bool
  = "iocp_submit_buffer"

external submit_accept : operation -> Unix.file_descr -> bool
  = "iocp_submit_accept"

external submit_connect : operation -> Unix.sockaddr -> bool
  = "iocp_submit_connect"

external cancel_operation : operation -> unit = "iocp_cancel"

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
