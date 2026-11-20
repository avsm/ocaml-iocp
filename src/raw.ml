(* Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>
   SPDX-License-Identifier: MIT *)

type access = Read | Write | Read_write

external reopen : Unix.file_descr -> access -> bool -> Unix.file_descr
  = "iocp_reopen"

external create_readonly :
  string -> access -> bool -> exclusive:bool -> truncate:bool -> Unix.file_descr
  = "iocp_create_readonly"
(** A later reopen for write access would be denied by the read-only attribute.
*)

external pipe_raw : bool -> bool -> Unix.file_descr * Unix.file_descr
  = "iocp_pipe"
