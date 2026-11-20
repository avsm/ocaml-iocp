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

// OCaml APIs
#define CAML_NAME_SPACE
#include <caml/bigarray.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/signals.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <caml/socketaddr.h>
#include <caml/osdeps.h>
#include <caml/fail.h>


// Windows APIs
#define WIN32_LEAN_AND_MEAN
#include <Fileapi.h>
#include <Minwinbase.h>
#include <Mswsock.h>
#include <bcrypt.h>
#include <winternl.h>
#include <stdint.h>
#include <stdio.h>
#include <assert.h>


#define SIZEBUF 4096

/*-----------------------------------------------------------------------
   Copyright (c) 2022 Patrick Ferris <patrick@sirref.org>
   Distributed under the MIT license. See terms further down in the file.
  -----------------------------------------------------------------------*/

// Overlapped data structure:
// Contains information for asynchronous (i.e. overlapped) input and output

typedef struct extended_overlapped {
  OVERLAPPED o;
  value key;
} eo;

static value val_of_overlapped_ptr(eo *ptr)
{
  assert(((uintptr_t)ptr & 1)==0);
  return (value) ptr | 1;
}

static LPOVERLAPPED overlapped_ptr_of_val(value v)
{
  assert(v != 0);
  assert(v & 1 == 1);
  return (LPOVERLAPPED) (v & ~1);
}

void ocaml_iocp_free_overlapped(value v) {
    CAMLparam1(v);
    eo *lp=(eo *)overlapped_ptr_of_val(Field(v,0));
    caml_stat_free(lp);
    CAMLreturn0;
}

CAMLprim value ocaml_iocp_alloc_overlapped(value key) {
    CAMLparam1(key);

    if(Is_block(key)) {
      caml_invalid_argument("Overlapped key must be an immediate");
    }

    eo *ol = (eo *) caml_stat_alloc(sizeof(eo));
    memset(ol, 0, sizeof(eo));

    ol->key = key;

    /* Box this so the OCaml side can set a finalizer */
    CAMLreturn(caml_alloc_boxed(val_of_overlapped_ptr(ol)));
}

void ocaml_iocp_set_overlapped_off(value v, value off) {
  CAMLparam2(v, off);
  LPOVERLAPPED ol=overlapped_ptr_of_val(Field(v,0));
  memset(ol, 0, sizeof(OVERLAPPED));
  ol->Offset = Int_val(off);
  ol->OffsetHigh = 0;
  CAMLreturn0;
}

value ocaml_iocp_get_overlapped_key(value v) {
  CAMLparam1(v);
  eo *ol = (eo *)overlapped_ptr_of_val(v);
  CAMLreturn(ol->key);
}

void ocaml_iocp_set_overlapped_key(value v, value key) {
  CAMLparam2(v, key);
  eo *ol = (eo *)overlapped_ptr_of_val(v);
  if(Is_block(key)) {
      caml_invalid_argument("Overlapped key must be an immediate");
  }
  ol->key = key;
  CAMLreturn0;
}

// The key cannot be GC'd until the event is complete
value ocaml_iocp_create_io_completion_port(value v_threads) {
    CAMLparam0();
    CAMLlocal1(v);
    int num_threads = Int_val(v_threads);

    HANDLE cp = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0 /* ignored */, num_threads);

    if (cp == NULL) {
      win32_maperr(GetLastError());
      // printf("RETURNING FAILED %i\n", GetLastError());
      uerror("CreateIoCompletionPort", Nothing);
    }

    v = win_alloc_handle(cp);

    Handle_val(v) = cp;
    CAMLreturn(v);
}

value ocaml_iocp_associate_fd_with_iocp(value v_iocp, value v_fd, value v_key) {
    CAMLparam3(v_iocp, v_fd, v_key);

    HANDLE handle = Handle_val(v_fd);
    HANDLE iocp = Handle_val(v_iocp);

    HANDLE cp = CreateIoCompletionPort(handle, iocp, v_key, 0);

    if (cp == NULL) {
      win32_maperr(GetLastError());
      // printf("RETURNING FAILED %i\n", GetLastError());
      uerror("CreateIoCompletionPort", Nothing);
    }

    CAMLreturn(v_fd);

}
// The key cannot be GC'd until the event is complete
value ocaml_iocp_create_io_completion_port_with_fd(value v_fd, value v_threads, value v_key) {
    CAMLparam3(v_fd, v_threads, v_key);
    CAMLlocal1(v);
    int num_threads = Int_val(v_threads);

    HANDLE handle = Handle_val(v_fd);
    HANDLE cp = CreateIoCompletionPort(handle, NULL, v_key, num_threads);

    if (cp == NULL) {
      win32_maperr(GetLastError());
      // printf("RETURNING FAILED %i\n", GetLastError());
      uerror("CreateIoCompletionPort", Nothing);
    }

    v = win_alloc_handle(cp);

    Handle_val(v) = cp;
    CAMLreturn(v);
}

#define Val_cqe_none Val_int(0)

static value Val_completion_status(value v_id, value v_bytes, value v_overlapped) {
    CAMLparam2(v_id, v_bytes);
    CAMLlocal1(v);
    v = caml_alloc(3, 0);
    Store_field(v, 0, v_id);
    Store_field(v, 1, v_bytes);
    Store_field(v, 2, v_overlapped);
    CAMLreturn(v);
}

value ocaml_iocp_get_queued_completion_status(value v_fd, value v_timeout) {
    CAMLparam2(v_fd, v_timeout);
    CAMLlocal2(v,v_err);
    BOOL b = 0;
    HANDLE fd = Handle_val(v_fd);
    DWORD transferred = 0;
    DWORD_PTR ptr;
    DWORD err;
    v_err = Val_int(0); /* None */
    LPOVERLAPPED ol = NULL;

    caml_enter_blocking_section();
    b = GetQueuedCompletionStatus(fd, &transferred, &ptr, &ol, Int_val(v_timeout));
    caml_leave_blocking_section();

    if (b && ol == NULL) {
      printf("Unknown status: returned 'true', but ol is null");
      CAMLreturn(Val_int(0)); /* success, nothing to return (hit timeout) */
    }

    /* Indicates an error with the call to GetQueuedCompletionStatus */
    if (!b && ol == NULL) {
      /* A timeout is an OK result, don't raise an exception */
      err=GetLastError();

      /* A timeout is an OK result, don't raise an exception.
         I have also seen ERROR_SUCCESS, so let's not raise
         an error for this! */
      if(err==WAIT_TIMEOUT || err==ERROR_SUCCESS) {
        CAMLreturn(Val_int(0));
      }

      /* For all other errors, raise a Unix_error */
      win32_maperr(err);
      // printf("RETURNING FAILED %i\n", err);
      uerror("QueuedCompletionStatus", Nothing);
    }

    /* Indicates an error with the IO operation represented by ol */
    if(!b && ol != NULL) {
      /* Set the global var errno */
      win32_maperr(GetLastError ());
      v_err = caml_alloc(1, 0);
      Store_field(v_err, 0, unix_error_of_code(errno));
    }

    v = caml_alloc(4, 0);
    Store_field(v, 0, Val_int(ptr));
    Store_field(v, 1, Val_int(transferred));
    Store_field(v, 2, val_of_overlapped_ptr((eo *)ol));
    Store_field(v, 3, v_err);
    CAMLreturn(v);
}

void ocaml_iocp_get_queued_completion_status_unsafe(value v_fd, value v_timeout, value v) {
    CAMLparam3(v_fd, v_timeout, v);
    BOOL b = 0;
    HANDLE fd = Handle_val(v_fd);
    DWORD transferred = 0;
    DWORD_PTR ptr;
    DWORD err=0;
    LPOVERLAPPED ol = NULL;

    // printf("ocaml_iocp_get_queued_completion_status_unsafe: here we go!\n");

    caml_enter_blocking_section();
    b = GetQueuedCompletionStatus(fd, &transferred, &ptr, &ol, Int_val(v_timeout));
    caml_leave_blocking_section();

    /* Indicates an error with the call to GetQueuedCompletionStatus */
    if (!b) {
      /* A timeout is an OK result, don't raise an exception */
      err=GetLastError();
    }

    Store_field(v, 0, Val_int(ptr));
    Store_field(v, 1, Val_int(transferred));
    Store_field(v, 2, val_of_overlapped_ptr((eo *)ol));
    Store_field(v, 3, Val_bool(b));
    Store_field(v, 4, Val_int(err));

    // printf("OK, ol=%p res=%d err=%d\n",(void *)ol,b,err);
    CAMLreturn0;
}

value ocaml_iocp_peek(value v_fd) {
    CAMLparam1(v_fd);
    CAMLlocal1(v);
    BOOL b = 0;
    HANDLE fd = Handle_val(v_fd);
    DWORD transferred = 0;
    DWORD_PTR ptr = 0 ;
    LPOVERLAPPED ol = NULL;

    caml_enter_blocking_section();
    b = GetQueuedCompletionStatus(fd, &transferred, &ptr, &ol, 0);
    caml_leave_blocking_section();

    if (!b && GetLastError() != ERROR_HANDLE_EOF) {
      // win32_maperr(GetLastError());
      // uerror("CreateNamedPipe", Nothing);
      //CAMLreturn(Val_cqe_none);
    }
    v = caml_alloc(3, 0);
    Store_field(v, 0, Val_int(ptr));
    Store_field(v, 1, Val_int(transferred));
    Store_field(v, 2, val_of_overlapped_ptr((eo *)ol));
    CAMLreturn(v);
}

void ocaml_iocp_read(value v_cp, value v_fd, value v_ba, value v_num_bytes, value v_off, value v_overlapped) {
    CAMLparam4(v_cp, v_fd, v_ba, v_overlapped);
    LPOVERLAPPED ol = overlapped_ptr_of_val(Field(v_overlapped,0));

    // printf("READ FILE %i %i %i %i\n", Handle_val(v_fd), Int_val(v_off), Int_val(v_num_bytes), ol->Offset);

    // Here we associate the file handle to the completion port handle...
    void *buf = Caml_ba_data_val(v_ba) + Long_val(v_off);
    BOOL b = ReadFile(Handle_val(v_fd), buf, Int_val(v_num_bytes), NULL, ol);
    // The return value is non-zero (TRUE) on success. However, it is FALSE if the IO operation
    // is completing asynchronously. We change that behaviour by checking last error.
    if (!b) {
      DWORD err = GetLastError();
      if(err == ERROR_IO_PENDING) {
        // printf("Returned pending\n");
        CAMLreturn0;
      }
      win32_maperr(err);
      uerror("ReadFile", Nothing);
    }
    CAMLreturn0;
}

void ocaml_iocp_read_bytes(value* values, int argc) {
    return ocaml_iocp_read(values[0], values[1], values[2], values[3], values[4], values[5]);
}

void ocaml_iocp_write(value v_cp, value v_fd, value v_ba, value v_num_bytes, value v_off, value v_overlapped) {
    CAMLparam5(v_cp, v_fd, v_ba, v_num_bytes, v_off);
    CAMLxparam1(v_overlapped);
    LPOVERLAPPED ol = overlapped_ptr_of_val(Field(v_overlapped,0));

    // printf("iocp_write - overlapped=%p\n\n",(void*) ol);
    // Here we associate the file handle to the completion port handle...
    void *buf = Caml_ba_data_val(v_ba) + Long_val(v_off);
    BOOL b = WriteFile(Handle_val(v_fd), buf, Int_val(v_num_bytes), NULL, ol);

    // The return value is non-zero (TRUE) on success. However, it is FALSE if the IO operation
    // is completing asynchronously. We change that behaviour by checking last error.
    if (!b) {
      DWORD err = GetLastError();
      if(err == ERROR_IO_PENDING) {
        // printf("Returned pending\n");
        CAMLreturn0;
      }
      win32_maperr(err);
      uerror("WriteFile", Nothing);
    }
    CAMLreturn0;
}

void ocaml_iocp_write_bytes(value* values, int argc) {
    return ocaml_iocp_write(values[0], values[1], values[2], values[3], values[4], values[5]);
}
value ocaml_iocp_cancel(value v_fd, value v_overlapped) {
  CAMLparam2(v_fd, v_overlapped);
  LPOVERLAPPED ol = overlapped_ptr_of_val(Field(v_overlapped,0));

  BOOL b;
  b = CancelIoEx(Handle_val(v_fd), ol);

  CAMLreturn(Val_unit);
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

CAMLextern value caml_win32_alloc_handle(HANDLE);
CAMLextern void caml_win32_maperr(DWORD);
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

/* Constant constructors of Raw.buffer_op come first, in declaration order. */
enum operation_kind { OP_READ, OP_WRITE };

typedef struct {
  OVERLAPPED overlapped; /* Must be first: completion packets return this address. */
  HANDLE fd;
  enum operation_kind kind;
  intnat id;
  value roots;
  int pending;
  DWORD bytes;
  DWORD error;
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
CAMLprim value iocp_allocate(value p, value id, value fd, value buffers) {
  CAMLparam4(p, id, fd, buffers);
  CAMLlocal2(v, storage);
  storage = caml_alloc_custom_mem(&request_ops, sizeof(request *), sizeof(request));
  *(request **)Data_custom_val(storage) = NULL;
  request *r = caml_stat_alloc(sizeof(request));
  memset(r, 0, sizeof(*r));
  *(request **)Data_custom_val(storage) = r;
  r->fd = Handle_val(fd);
  r->id = Long_val(id);
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
  if (r->kind == OP_READ && (err == ERROR_HANDLE_EOF || err == ERROR_BROKEN_PIPE)) {
    err = 0;
    bytes = 0;
  }
  r->bytes = bytes;
  r->error = err;
}

static value completion(request *r) {
  CAMLparam0();
  CAMLlocal3(result, error, code);
  DWORD err = r->error, bytes = r->bytes;
  error = Val_none;
  if (err) {
    caml_win32_maperr(err);
    code = caml_unix_error_of_code(errno);
    error = caml_alloc_some(code);
  }
  result = caml_alloc_tuple(3);
  Store_field(result, 0, Val_long(r->id));
  Store_field(result, 1, Val_long(bytes));
  Store_field(result, 2, error);
  CAMLreturn(caml_alloc_some(result));
}
static value submitted(request *r, DWORD err, const char *op) {
  if (err != ERROR_SUCCESS && err != ERROR_IO_PENDING) {
    finish(r);
    raise_win32(err, op);
  }
  return Val_unit;
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
  r->kind = Int_val(kind);
  DWORD err = 0;
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
  }
  if (r->kind == OP_READ && (err == ERROR_HANDLE_EOF || err == ERROR_BROKEN_PIPE)) {
    if (PostQueuedCompletionStatus(port(Field(v, 1)), 0, 0, &r->overlapped))
      CAMLreturn(Val_unit);
    else
      err = GetLastError();
  }
  CAMLreturn(submitted(r, err, "overlapped_io"));
}
CAMLprim value iocp_completion(value v) {
  CAMLparam1(v);
  CAMLlocal1(result);
  request *r = Request(v);
  result = completion(r);
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
