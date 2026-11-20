/*
 * Copyright (C) 2020-2021 Anil Madhavapeddy
 * Copyright (C) 2020-2021 Sadiq Jaffer
 * Copyright (C) 2026 Anil Madhavapeddy <anil@recoil.org>
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
 */

#define WIN32_LEAN_AND_MEAN
#include <winsock2.h>
#include <windows.h>
#include <winternl.h>
#include <bcrypt.h>
#include <stdint.h>
#include <errno.h>
#include <assert.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/osdeps.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/bigarray.h>
#include <caml/signals.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>

CAMLextern value caml_win32_alloc_handle(HANDLE);
CAMLextern void caml_win32_maperr(DWORD);
CAMLextern void caml_unix_check_path(value, const char *);
CAMLextern value caml_unix_error_of_code(int);
CAMLnoret CAMLextern void caml_uerror(const char *, value);

static void raise_win32(DWORD err, const char *name) {
  caml_win32_maperr(err);
  caml_uerror(name, Nothing);
}
/*---------------------------------------------------------------------------
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
  ---------------------------------------------------------------------------*/

static DWORD file_access(value access, value append) {
  DWORD rights = Int_val(access) == 0   ? GENERIC_READ
                 : Int_val(access) == 1 ? GENERIC_WRITE
                                        : GENERIC_READ | GENERIC_WRITE;
  if (Bool_val(append) && Int_val(access) != 0)
    rights = (rights & ~GENERIC_WRITE) | FILE_APPEND_DATA;
  return rights;
}

static DWORD share_mode(value share_delete) {
  return FILE_SHARE_READ | FILE_SHARE_WRITE | (Bool_val(share_delete) ? FILE_SHARE_DELETE : 0);
}

CAMLprim value iocp_reopen(value fd, value access, value append, value share_delete) {
  CAMLparam4(fd, access, append, share_delete);
  CAMLlocal1(result);
  result = caml_win32_alloc_handle(INVALID_HANDLE_VALUE);
  HANDLE original = Handle_val(fd);
  DWORD rights = file_access(access, append);
  DWORD sharing = share_mode(share_delete);
  caml_enter_blocking_section();
  HANDLE h = ReOpenFile(original, rights, sharing,
                        FILE_FLAG_OVERLAPPED | FILE_FLAG_OPEN_REPARSE_POINT);
  DWORD err = h == INVALID_HANDLE_VALUE ? GetLastError() : 0;
  caml_leave_blocking_section();
  if (err)
    raise_win32(err, "ReOpenFile");
  Handle_val(result) = h;
  CAMLreturn(result);
}

CAMLprim value iocp_create_readonly(value path, value access, value append,
                                   value share_delete, value creation) {
  CAMLparam5(path, access, append, share_delete, creation);
  CAMLlocal1(result);
  result = caml_win32_alloc_handle(INVALID_HANDLE_VALUE);
  caml_unix_check_path(path, "open");
  DWORD rights = file_access(access, append);
  static const DWORD dispositions[] = {CREATE_NEW, CREATE_ALWAYS, OPEN_ALWAYS};
  DWORD disposition = dispositions[Int_val(creation)], sharing = share_mode(share_delete);
  wchar_t *name = caml_stat_strdup_to_utf16(String_val(path));
  caml_enter_blocking_section_no_pending();
  HANDLE h = CreateFileW(name, rights, sharing, NULL,
                         disposition, FILE_ATTRIBUTE_READONLY | FILE_FLAG_OVERLAPPED, NULL);
  DWORD err = h == INVALID_HANDLE_VALUE ? GetLastError() : 0;
  caml_stat_free(name);
  caml_leave_blocking_section();
  if (err)
    raise_win32(err, "CreateFileW");
  Handle_val(result) = h;
  CAMLreturn(result);
}
