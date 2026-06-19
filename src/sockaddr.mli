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

(* Borrowed from the IO_URING bindings *)

type t
(** A socket address, stored off the OCaml heap so it can be filled in by an
    asynchronous operation. *)

val of_unix : Unix.sockaddr -> t
val create : unit -> t
val get : t -> Unix.sockaddr

(** {2 AcceptEx address buffer}

    AcceptEx requires a caller-provided buffer to receive the new connection's
    local and remote addresses. *)

val accept_buffer : unit -> Cstruct.t
(** A correctly-sized scratch buffer to pass to {!Iocp.accept}. Keep it alive
    until the accept completes, then recover the peer with {!of_accept_buffer}. *)

val of_accept_buffer : Cstruct.t -> listen:Handle.t -> t
(** [of_accept_buffer buf ~listen] extracts the peer address AcceptEx wrote into
    [buf]. [listen] is the listening socket the connection was accepted on. *)
