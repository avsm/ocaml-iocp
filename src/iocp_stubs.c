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
#include <mswsock.h>
#include <winternl.h>
#include <bcrypt.h>
#include <stdint.h>
#include <errno.h>
#include <assert.h>
#include <caml/mlvalues.h>
#include <caml/osdeps.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/bigarray.h>
#include <caml/signals.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>
#include <caml/socketaddr.h>

CAMLextern value caml_win32_alloc_handle(HANDLE);
CAMLextern void caml_win32_maperr(DWORD);
CAMLextern value caml_unix_error_of_code(int);
#ifdef caml_unix_alloc_sockaddr
CAMLextern value(caml_unix_alloc_sockaddr)(struct sockaddr *, socklen_t, int);
#else
CAMLextern value caml_unix_alloc_sockaddr(union sock_addr_union *, socklen_param_type, int);
#endif
CAMLnoret CAMLextern void caml_uerror(const char *, value);
#ifdef caml_unix_get_sockaddr
CAMLextern void(caml_unix_get_sockaddr)(value, struct sockaddr_storage *, socklen_t *);
#else
CAMLextern void caml_unix_get_sockaddr(value, union sock_addr_union *, socklen_param_type *);
#endif

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
CAMLprim value iocp_enable_immediate(value fd) {
  if (Descr_kind_val(fd) == KIND_SOCKET) {
    WSAPROTOCOL_INFOW info;
    int len = sizeof(info);
    if (getsockopt(Socket_val(fd), SOL_SOCKET, SO_PROTOCOL_INFOW, (char *)&info, &len) ==
            SOCKET_ERROR)
      raise_win32(WSAGetLastError(), "getsockopt");
    if (!(info.dwServiceFlags1 & XP1_IFS_HANDLES))
      return Val_false;
  }
  if (!SetFileCompletionNotificationModes(Handle_val(fd), FILE_SKIP_COMPLETION_PORT_ON_SUCCESS))
    raise_win32(GetLastError(), "SetFileCompletionNotificationModes");
  return Val_true;
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

/*-----------------------------------------------------------------------
   Copyright (c) 2022 Patrick Ferris <patrick@sirref.org>
   Distributed under the MIT license. See terms further down in the file.
  -----------------------------------------------------------------------*/

/* Matches Raw.delivery. */
enum delivery { NATIVE_QUEUE, POST_QUEUE, IMMEDIATE };

/* Constant constructors of Raw.buffer_op come first, in declaration order. */
enum operation_kind { OP_READ, OP_WRITE, OP_RECV, OP_SEND, OP_RECVFROM, OP_SENDTO, OP_ACCEPT, OP_CONNECT };

typedef struct {
  OVERLAPPED overlapped; /* Must be first: completion packets return this address. */
  HANDLE fd;
  SOCKET client;
  enum operation_kind kind;
  intnat id;
  value roots;
  int pending;
  DWORD bytes;
  enum delivery delivery;
  DWORD error;
  int address_len;
  DWORD flags;
  union {
    char accept[2 * (sizeof(SOCKADDR_STORAGE) + 16)];
    union sock_addr_union connect;
  } addresses;
} request;
#define Request(v) (*(request **)Data_custom_val(Field(v, 0)))
static void finalize_request(value v) {
  request *r = *(request **)Data_custom_val(v);
  if (r != NULL) {
    assert(!r->pending);
    caml_stat_free(r);
  }
}
static struct custom_operations request_ops = {"iocp.request",
                                               finalize_request,
                                               custom_compare_default,
                                               custom_hash_default,
                                               custom_serialize_default,
                                               custom_deserialize_default,
                                               custom_compare_ext_default,
                                               custom_fixed_length_default};
CAMLprim value iocp_allocate(value p, value id, value fd, value buffers, value delivery) {
  CAMLparam5(p, id, fd, buffers, delivery);
  CAMLlocal2(v, storage);
  storage = caml_alloc_custom_mem(&request_ops, sizeof(request *), sizeof(request));
  *(request **)Data_custom_val(storage) = NULL;
  request *r = caml_stat_alloc(sizeof(request));
  memset(r, 0, sizeof(*r));
  *(request **)Data_custom_val(storage) = r;
  r->fd = Handle_val(fd);
  r->id = Long_val(id);
  r->delivery = Int_val(delivery);
  v = caml_alloc_tuple(4);
  Store_field(v, 0, storage);
  Store_field(v, 1, p);
  Store_field(v, 2, fd);
  Store_field(v, 3, buffers);
  CAMLreturn(v);
}
static void start(value v) {
  request *r = Request(v);
  r->roots = v;
  caml_register_generational_global_root(&r->roots);
  r->pending = 1;
}
static void finish(request *r) {
  Store_field(r->roots, 3, Val_unit);
  r->pending = 0;
  caml_remove_generational_global_root(&r->roots);
}
static void record_result(request *r, DWORD bytes, DWORD err) {
  if (!err) err = r->error;
  if (err && r->kind >= OP_RECV) {
    DWORD flags = 0;
    if (!WSAGetOverlappedResult((SOCKET)r->fd, &r->overlapped, &bytes, FALSE, &flags))
      err = WSAGetLastError();
  }
  if ((r->kind == OP_READ && (err == ERROR_HANDLE_EOF || err == ERROR_BROKEN_PIPE)) ||
      (r->kind == OP_RECV && err == WSAESHUTDOWN)) {
    err = 0;
    bytes = 0;
  }
  if (!err && r->kind == OP_ACCEPT) {
    SOCKET listener = (SOCKET)r->fd;
    if (setsockopt(r->client, SOL_SOCKET, SO_UPDATE_ACCEPT_CONTEXT, (char *)&listener,
                   sizeof(listener)) == SOCKET_ERROR)
      err = WSAGetLastError();
  }
  if (!err && r->kind == OP_CONNECT &&
      setsockopt((SOCKET)r->fd, SOL_SOCKET, SO_UPDATE_CONNECT_CONTEXT, NULL, 0) == SOCKET_ERROR)
    err = WSAGetLastError();
  r->bytes = bytes;
  r->error = err;
}

static value completion(request *r, int eof_on_cancel) {
  CAMLparam0();
  CAMLlocal4(result, error, code, source);
  DWORD err = r->error, bytes = r->bytes;
  if (eof_on_cancel && err == ERROR_OPERATION_ABORTED) {
    err = 0;
    bytes = 0;
  }
  source = Val_none;
  if (r->kind == OP_RECVFROM && (!r->error || r->error == WSAEMSGSIZE)) {
    source = caml_unix_alloc_sockaddr(&r->addresses.connect, r->address_len, -1);
    source = caml_alloc_some(source);
  }
  error = Val_none;
  if (err) {
    caml_win32_maperr(err);
    code = caml_unix_error_of_code(errno);
    error = caml_alloc_some(code);
  }
  result = caml_alloc_tuple(4);
  Store_field(result, 0, Val_long(r->id));
  Store_field(result, 1, Val_long(bytes));
  Store_field(result, 2, error);
  Store_field(result, 3, source);
  CAMLreturn(caml_alloc_some(result));
}
static value complete_immediately(request *r, DWORD bytes, DWORD err) {
  record_result(r, bytes, err);
  finish(r);
  return Val_true;
}

static value submitted(request *r, DWORD err, const char *op) {
  if (err != ERROR_SUCCESS && err != ERROR_IO_PENDING) {
    finish(r);
    raise_win32(err, op);
  }
  if (!err && r->delivery != NATIVE_QUEUE) {
    DWORD bytes = 0, flags = 0;
    if (r->kind >= OP_RECV) {
      if (!WSAGetOverlappedResult((SOCKET)r->fd, &r->overlapped, &bytes, FALSE, &flags))
        err = WSAGetLastError();
    } else if (!GetOverlappedResult(r->fd, &r->overlapped, &bytes, FALSE))
      err = GetLastError();
    if (r->delivery == IMMEDIATE)
      return complete_immediately(r, bytes, err);
    r->error = err;
    if (!PostQueuedCompletionStatus(port(Field(r->roots, 1)), bytes, 0, &r->overlapped)) {
      DWORD post_error = GetLastError();
      finish(r);
      raise_win32(post_error, "PostQueuedCompletionStatus");
    }
  }
  return Val_false;
}
CAMLprim value iocp_submit_buffer(value v, value kind, value buf, value offset) {
  CAMLparam4(v, kind, buf, offset);
  request *r = Request(v);
  value ba = Field(buf, 0);
  intnat pos = Long_val(Field(buf, 1)), len = Long_val(Field(buf, 2));
  intnat size = Caml_ba_array_val(ba)->dim[0];
  if (pos < 0 || len < 0 || pos > size || len > size - pos || (uintnat)len > MAXDWORD)
    caml_invalid_argument("Iocp: buffer bounds");
  char *data = (char *)Caml_ba_data_val(ba) + pos;
  uint64_t off = (uint64_t)Int64_val(offset);
  r->overlapped.Offset = (DWORD)off;
  r->overlapped.OffsetHigh = (DWORD)(off >> 32);
  r->kind = Is_block(kind) ? OP_SENDTO : Int_val(kind);
  if (r->kind == OP_RECVFROM)
    r->address_len = sizeof(r->addresses.connect);
  if (r->kind == OP_SENDTO) {
    socklen_param_type len;
    caml_unix_get_sockaddr(Field(kind, 0), &r->addresses.connect, &len);
    r->address_len = len;
  }
  DWORD err = 0;
  WSABUF b = {(ULONG)len, data};
  start(v);
  switch (r->kind) {
  case OP_READ:
    if (!ReadFile(r->fd, data, (DWORD)len, NULL, &r->overlapped))
      err = GetLastError();
    break;
  case OP_WRITE:
    if (!WriteFile(r->fd, data, (DWORD)len, NULL, &r->overlapped))
      err = GetLastError();
    break;
  case OP_RECV:
    if (WSARecv((SOCKET)r->fd, &b, 1, NULL, &r->flags, &r->overlapped, NULL) == SOCKET_ERROR)
      err = WSAGetLastError();
    break;
  case OP_SEND:
    if (WSASend((SOCKET)r->fd, &b, 1, NULL, 0, &r->overlapped, NULL) == SOCKET_ERROR)
      err = WSAGetLastError();
    break;
  case OP_RECVFROM:
    if (WSARecvFrom((SOCKET)r->fd, &b, 1, NULL, &r->flags, &r->addresses.connect.s_gen,
                    &r->address_len, &r->overlapped, NULL) == SOCKET_ERROR)
      err = WSAGetLastError();
    break;
  case OP_SENDTO:
    if (WSASendTo((SOCKET)r->fd, &b, 1, NULL, 0, &r->addresses.connect.s_gen, r->address_len,
                  &r->overlapped, NULL) == SOCKET_ERROR)
      err = WSAGetLastError();
    break;
  default:
    finish(r);
    caml_invalid_argument("Iocp: invalid buffer operation");
  }
  if ((r->kind == OP_READ && (err == ERROR_HANDLE_EOF || err == ERROR_BROKEN_PIPE)) ||
      (r->kind == OP_RECV && err == WSAESHUTDOWN)) {
    if (r->delivery == IMMEDIATE)
      CAMLreturn(complete_immediately(r, 0, 0));
    if (PostQueuedCompletionStatus(port(Field(v, 1)), 0, 0, &r->overlapped))
      CAMLreturn(Val_false);
    else
      err = GetLastError();
  }
  CAMLreturn(submitted(r, err, "overlapped_io"));
}
CAMLprim value iocp_submit_accept(value v, value client) {
  CAMLparam2(v, client);
  request *r = Request(v);
  GUID guid = WSAID_ACCEPTEX;
  LPFN_ACCEPTEX fn;
  DWORD bytes;
  if (WSAIoctl((SOCKET)r->fd, SIO_GET_EXTENSION_FUNCTION_POINTER, &guid, sizeof(guid), &fn,
               sizeof(fn), &bytes, NULL, NULL) == SOCKET_ERROR)
    raise_win32(WSAGetLastError(), "AcceptEx lookup");
  r->kind = OP_ACCEPT;
  r->client = Socket_val(client);
  start(v);
  BOOL ok = fn((SOCKET)r->fd, r->client, r->addresses.accept, 0, sizeof(SOCKADDR_STORAGE) + 16,
               sizeof(SOCKADDR_STORAGE) + 16, &bytes, &r->overlapped);
  CAMLreturn(submitted(r, ok ? 0 : WSAGetLastError(), "AcceptEx"));
}
CAMLprim value iocp_submit_connect(value v, value addr) {
  CAMLparam2(v, addr);
  request *r = Request(v);
  GUID guid = WSAID_CONNECTEX;
  LPFN_CONNECTEX fn;
  DWORD bytes;
  socklen_param_type len;
  caml_unix_get_sockaddr(addr, &r->addresses.connect, &len);
  if (WSAIoctl((SOCKET)r->fd, SIO_GET_EXTENSION_FUNCTION_POINTER, &guid, sizeof(guid), &fn,
               sizeof(fn), &bytes, NULL, NULL) == SOCKET_ERROR)
    raise_win32(WSAGetLastError(), "ConnectEx lookup");
  r->kind = OP_CONNECT;
  start(v);
  BOOL ok = fn((SOCKET)r->fd, &r->addresses.connect.s_gen, len, NULL, 0, &bytes, &r->overlapped);
  CAMLreturn(submitted(r, ok ? 0 : WSAGetLastError(), "ConnectEx"));
}
CAMLprim value iocp_completion(value v, value eof_on_cancel) {
  CAMLparam2(v, eof_on_cancel);
  CAMLlocal1(result);
  request *r = Request(v);
  result = completion(r, Bool_val(eof_on_cancel));
  if (r->pending)
    finish(r);
  CAMLreturn(result);
}

CAMLprim value iocp_cancel(value v) {
  request *r = Request(v);
  if (!CancelIoEx(r->fd, &r->overlapped)) {
    DWORD err = GetLastError();
    if (err != ERROR_NOT_FOUND)
      raise_win32(err, "CancelIoEx");
  }
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
    if (ol) {
      request *r = (request *)ol;
      record_result(r, bytes, err);
      state->ready_id = r->id;
    } else if (ok) {
      state->ready_id = 0;
    }
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

CAMLprim value iocp_reopen(value fd, value access, value append) {
  CAMLparam3(fd, access, append);
  CAMLlocal1(result);
  result = caml_win32_alloc_handle(INVALID_HANDLE_VALUE);
  HANDLE original = Handle_val(fd);
  DWORD rights = file_access(access, append);
  caml_enter_blocking_section();
  HANDLE h = ReOpenFile(original, rights, FILE_SHARE_READ | FILE_SHARE_WRITE,
                        FILE_FLAG_OVERLAPPED | FILE_FLAG_OPEN_REPARSE_POINT);
  DWORD err = h == INVALID_HANDLE_VALUE ? GetLastError() : 0;
  caml_leave_blocking_section();
  if (err)
    raise_win32(err, "ReOpenFile");
  Handle_val(result) = h;
  CAMLreturn(result);
}

CAMLprim value iocp_create_readonly(value path, value access, value append,
                                   value exclusive, value truncate) {
  CAMLparam5(path, access, append, exclusive, truncate);
  CAMLlocal1(result);
  result = caml_win32_alloc_handle(INVALID_HANDLE_VALUE);
  caml_unix_check_path(path, "open");
  DWORD rights = file_access(access, append);
  DWORD disposition = Bool_val(exclusive) ? CREATE_NEW : Bool_val(truncate) ? CREATE_ALWAYS : OPEN_ALWAYS;
  wchar_t *name = caml_stat_strdup_to_utf16(String_val(path));
  caml_enter_blocking_section_no_pending();
  HANDLE h = CreateFileW(name, rights, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL,
                         disposition, FILE_ATTRIBUTE_READONLY | FILE_FLAG_OVERLAPPED, NULL);
  DWORD err = h == INVALID_HANDLE_VALUE ? GetLastError() : 0;
  caml_stat_free(name);
  caml_leave_blocking_section();
  if (err)
    raise_win32(err, "CreateFileW");
  Handle_val(result) = h;
  CAMLreturn(result);
}

CAMLprim value iocp_pipe(value read_async, value write_async) {
  CAMLparam2(read_async, write_async);
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
  caml_enter_blocking_section();
  HANDLE r = CreateNamedPipeW(name, PIPE_ACCESS_INBOUND | FILE_FLAG_FIRST_PIPE_INSTANCE | rf,
                              PIPE_TYPE_BYTE | PIPE_WAIT | PIPE_REJECT_REMOTE_CLIENTS, 1, 65536,
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

CAMLprim value iocp_wakeup_wait(value p, value timeout) {
  CAMLparam2(p, timeout);
  HANDLE h = port(p);
  DWORD ms = (DWORD)Long_val(timeout);
  DWORD bytes;
  ULONG_PTR key;
  OVERLAPPED *overlapped;
  caml_enter_blocking_section();
  BOOL ok = GetQueuedCompletionStatus(h, &bytes, &key, &overlapped, ms);
  DWORD err = ok ? 0 : GetLastError();
  caml_leave_blocking_section();
  if (err && err != WAIT_TIMEOUT)
    raise_win32(err, "GetQueuedCompletionStatus");
  CAMLreturn(Val_unit);
}
