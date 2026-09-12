(*
 * Copyright (C) 2020-2026 Anil Madhavapeddy
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type t

external of_unix : Unix.sockaddr -> t = "ocaml_iocp_make_sockaddr"
external get : t -> Unix.sockaddr = "ocaml_iocp_extract_sockaddr"

let dummy_addr = Unix.ADDR_UNIX "-"
let create () = of_unix dummy_addr

external accept_buffer_size : unit -> int = "ocaml_iocp_accept_buffer_size"

let accept_buffer () = Cstruct.create (accept_buffer_size ())

external get_accept_ex_sockaddr : Cstruct.buffer -> Handle.t -> t -> unit
  = "ocaml_iocp_get_accept_ex_sockaddr"

let of_accept_buffer buf ~listen =
  let s = create () in
  get_accept_ex_sockaddr (Cstruct.to_bigarray buf) listen s;
  s
