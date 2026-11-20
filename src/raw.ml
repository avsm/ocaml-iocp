(* Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>
   SPDX-License-Identifier: MIT *)

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
