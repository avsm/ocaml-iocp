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

value ocaml_iocp_unix_pipe(value v_iocp, value v_id1, value v_id2, value v_path)
{
  CAMLparam4(v_iocp, v_id1, v_id2, v_path);
  CAMLlocal2(readfd, writefd);
  value res;
  char *wpath = caml_stat_strdup(String_val(v_path));
  HANDLE iocp = Handle_val(v_iocp);

  SECURITY_ATTRIBUTES attr;
  attr.nLength = sizeof(attr);
  attr.lpSecurityDescriptor = NULL;
  attr.bInheritHandle = TRUE;

  HANDLE pipeR = CreateNamedPipe(
        TEXT(wpath), // name of the pipe
        (PIPE_ACCESS_DUPLEX | FILE_FLAG_OVERLAPPED),
        (PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT),
        1, // only allow 1 instance of this pipe
        SIZEBUF,
        SIZEBUF,
        PIPE_WAIT, // use default wait time
        &attr // use default security attributes
    );

  caml_stat_free(wpath);

  if (pipeR == INVALID_HANDLE_VALUE || pipeR == NULL) {
    win32_maperr(GetLastError());
    uerror("CreateNamedPipe", Nothing);
  }

  HANDLE pipeW = CreateFile(
        TEXT(wpath),
        GENERIC_READ | GENERIC_WRITE,
        FILE_SHARE_READ | FILE_SHARE_WRITE,
        NULL,
        OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL | FILE_FLAG_OVERLAPPED,
        &attr);

  if (pipeW == INVALID_HANDLE_VALUE || pipeW == NULL) {
    printf("Write handle failed\n");
    win32_maperr(GetLastError());
    uerror("CreateNamedPipe", Nothing);
  }

  HANDLE t = CreateIoCompletionPort(pipeR, iocp, v_id1, 0);

  if (t == NULL) {
      win32_maperr(GetLastError());
      // printf("RETURNING FAILED %i\n", GetLastError());
      uerror("CreateIoCompletionPort1", Nothing);
    }

  t = CreateIoCompletionPort(pipeW, iocp, v_id2, 0);

  if (t == NULL) {
      win32_maperr(GetLastError());
      // printf("RETURNING FAILED %i\n", GetLastError());
      uerror("CreateIoCompletionPort2", Nothing);
    }

  writefd = win_alloc_handle(pipeW);
  readfd = win_alloc_handle(pipeR);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = readfd;
  Field(res, 1) = writefd;
  CAMLreturn(res);
}

CAMLextern value caml_win32_alloc_handle(HANDLE);
CAMLextern void caml_win32_maperr(DWORD);
CAMLnoret CAMLextern void caml_uerror(const char *, value);
static void raise_win32(DWORD err, const char *name) {
  caml_win32_maperr(err);
  caml_uerror(name, Nothing);
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
