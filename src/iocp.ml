(*----------------------------------------------------------------------
   Copyright (c) 2022 Patrick Ferris <patrick@sirref.org>
   Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>
   Distributed under the MIT license. See terms at the end of this file.
  ----------------------------------------------------------------------*)

let openfile path flags perm =
  let has f = List.mem f flags in
  List.iter
    (function
      | Unix.O_RDONLY | O_WRONLY | O_RDWR | O_APPEND | O_CREAT | O_TRUNC
      | O_EXCL | O_CLOEXEC | O_KEEPEXEC | O_SHARE_DELETE ->
          ()
      | _ -> invalid_arg "Iocp.openfile: unsupported flag")
    flags;
  let mode, access =
    if has Unix.O_RDWR then (Unix.O_RDWR, Raw.Read_write)
    else if has O_WRONLY then (Unix.O_WRONLY, Raw.Write)
    else (Unix.O_RDONLY, Raw.Read)
  in
  let open_flags =
    List.filter
      (function
        | Unix.O_RDONLY | O_WRONLY | O_RDWR | O_APPEND | O_CLOEXEC | O_KEEPEXEC
          ->
            false
        | _ -> true)
      flags
  in
  let open_flags = Unix.O_CLOEXEC :: mode :: open_flags in
  let append = has O_APPEND and share_delete = has O_SHARE_DELETE in
  let fd =
    if access <> Raw.Read && has O_CREAT && perm land 0o200 = 0 then
      let disposition =
        if has O_EXCL then Raw.Create_new else if has O_TRUNC then Create_always else Open_always
      in
      Raw.create_readonly path access ~append ~share_delete disposition
    else
      let original = Unix.openfile path open_flags perm in
      Fun.protect
        ~finally:(fun () -> Unix.close original)
        (fun () -> Raw.reopen original access ~append ~share_delete)
  in
  try
    if has O_KEEPEXEC && not (has O_CLOEXEC) then Unix.clear_close_on_exec fd;
    fd
  with ex ->
    Unix.close fd;
    raise ex

(*---------------------------------------------------------------------------
  Copyright (c) 2022 <patrick@sirref.org>

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.
  ---------------------------------------------------------------------------*)
