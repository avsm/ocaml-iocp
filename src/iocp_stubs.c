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
/* Windows may dequeue a packet while the OCaml runtime lock is released. */
typedef struct {
  HANDLE handle;
  intnat ready_id; /* -1 for an empty slot, 0 for a wake notification. */
} port_state;
#define State(v) (*(port_state **)Data_custom_val(v))
#define Port(v) (State(v)->handle)
static void finalize_port(value v) {
  if (State(v) != NULL) {
    if (Port(v) != NULL)
      CloseHandle(Port(v));
    caml_stat_free(State(v));
  }
}
static struct custom_operations port_ops = {"iocp.port",
                                            finalize_port,
                                            custom_compare_default,
                                            custom_hash_default,
                                            custom_serialize_default,
                                            custom_deserialize_default,
                                            custom_compare_ext_default,
                                            custom_fixed_length_default};
static HANDLE port(value v) {
  if (Port(v) == NULL)
    caml_invalid_argument("Iocp: closed port");
  return Port(v);
}
CAMLprim value iocp_create(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(v);
  v = caml_alloc_custom_mem(&port_ops, sizeof(port_state *), sizeof(port_state));
  State(v) = NULL;
  port_state *state = caml_stat_alloc(sizeof(*state));
  State(v) = state;
  state->ready_id = -1;
  Port(v) = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, MAXDWORD);
  if (Port(v) == NULL)
    raise_win32(GetLastError(), "CreateIoCompletionPort");
  CAMLreturn(v);
}
CAMLprim value iocp_close(value v) {
  if (Port(v) != NULL) {
    if (!CloseHandle(Port(v)))
      raise_win32(GetLastError(), "CloseHandle");
    Port(v) = NULL;
  }
  return Val_unit;
}
CAMLprim value iocp_same_handle(value a, value b) {
  typedef BOOL (WINAPI *compare_fn)(HANDLE, HANDLE);
  compare_fn compare = (compare_fn)GetProcAddress(
      GetModuleHandleW(L"kernelbase.dll"), "CompareObjectHandles");
  if (!compare)
    caml_failwith("Iocp: CompareObjectHandles is unavailable");
  return Val_bool(compare(Handle_val(a), Handle_val(b)));
}
CAMLprim value iocp_associate(value v, value fd) {
  if (!CreateIoCompletionPort(Handle_val(fd), port(v), 0, 0))
    raise_win32(GetLastError(), "CreateIoCompletionPort");
  return Val_unit;
}

NTSYSAPI NTSTATUS NTAPI NtSetInformationFile(HANDLE, PIO_STATUS_BLOCK, PVOID, ULONG,
                                           FILE_INFORMATION_CLASS);
NTSYSAPI ULONG NTAPI RtlNtStatusToDosError(NTSTATUS);
CAMLprim value iocp_detach(value fd) {
  struct { HANDLE port; PVOID key; } info = {NULL, NULL};
  IO_STATUS_BLOCK io;
  /* FileReplaceCompletionInformation is absent from MinGW's winternl.h. */
  NTSTATUS status = NtSetInformationFile(Handle_val(fd), &io, &info, sizeof(info),
                                       (FILE_INFORMATION_CLASS)61);
  if (status < 0)
    raise_win32(RtlNtStatusToDosError(status), "NtSetInformationFile");
  return Val_unit;
}
CAMLprim value iocp_wake(value v) {
  if (!PostQueuedCompletionStatus(port(v), 0, 0, NULL))
    raise_win32(GetLastError(), "PostQueuedCompletionStatus");
  return Val_unit;
}

CAMLprim value iocp_dequeue(value p, value timeout) {
  CAMLparam2(p, timeout);
  port_state *state = State(p);
  if (state->ready_id < 0) {
    DWORD bytes, ms = (DWORD)Long_val(timeout);
    ULONG_PTR key;
    OVERLAPPED *ol = NULL;
    caml_enter_blocking_section();
    BOOL ok = GetQueuedCompletionStatus(state->handle, &bytes, &key, &ol, ms);
    DWORD err = ok ? 0 : GetLastError();
    if (ok) state->ready_id = 0;
    caml_leave_blocking_section();
    if (!ol && !ok && err != WAIT_TIMEOUT)
      raise_win32(err, "GetQueuedCompletionStatus");
  }
  CAMLreturn(Val_long(state->ready_id));
}

CAMLprim value iocp_acknowledge(value p) {
  State(p)->ready_id = -1;
  return Val_unit;
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

CAMLprim value iocp_pipe(value read_async, value write_async, value message) {
  CAMLparam3(read_async, write_async, message);
  CAMLlocal3(result, rd, wr);
  rd = caml_win32_alloc_handle(INVALID_HANDLE_VALUE);
  wr = caml_win32_alloc_handle(INVALID_HANDLE_VALUE);
  result = caml_alloc_tuple(2);
  Store_field(result, 0, rd);
  Store_field(result, 1, wr);
  unsigned long long random[2];
  NTSTATUS status =
      BCryptGenRandom(NULL, (PUCHAR)random, sizeof(random), BCRYPT_USE_SYSTEM_PREFERRED_RNG);
  if (status != 0)
    caml_failwith("Iocp.pipe: BCryptGenRandom failed");
  wchar_t name[96];
  swprintf(name, 96, L"\\\\.\\pipe\\ocaml-iocp-%016llx%016llx", random[0], random[1]);
  DWORD rf = Bool_val(read_async) ? FILE_FLAG_OVERLAPPED : 0;
  DWORD wf = Bool_val(write_async) ? FILE_FLAG_OVERLAPPED : 0;
  DWORD mode = Bool_val(message) ? PIPE_TYPE_MESSAGE | PIPE_READMODE_MESSAGE : PIPE_TYPE_BYTE;
  caml_enter_blocking_section();
  HANDLE r = CreateNamedPipeW(name, PIPE_ACCESS_INBOUND | FILE_FLAG_FIRST_PIPE_INSTANCE | rf,
                              mode | PIPE_WAIT | PIPE_REJECT_REMOTE_CLIENTS, 1, 65536,
                              65536, 0, NULL);
  DWORD err = r == INVALID_HANDLE_VALUE ? GetLastError() : 0;
  HANDLE w = INVALID_HANDLE_VALUE;
  if (!err) {
    w = CreateFileW(name, GENERIC_WRITE, 0, NULL, OPEN_EXISTING, wf, NULL);
    if (w == INVALID_HANDLE_VALUE) {
      err = GetLastError();
      CloseHandle(r);
    }
  }
  caml_leave_blocking_section();
  if (err)
    raise_win32(err, "Iocp.pipe");
  Handle_val(rd) = r;
  Handle_val(wr) = w;
  CAMLreturn(result);
}
