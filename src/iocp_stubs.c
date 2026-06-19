/*
 * Copyright (C) 2020-2021 Anil Madhavapeddy
 * Copyright (C) 2020-2021 Sadiq Jaffer
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
#include <assert.h>

/* Force these (header-declared plain [extern]) through the import table: as
   REL32 calls flexdll can't relocate them when the runtime DLL maps >2GB away.
   Most are reached via the unprefixed compatibility macros the caml headers
   #define to these caml_*-prefixed symbols: win32_maperr, uerror, win_alloc_handle,
   unix_error_of_code, and the get_sockaddr / alloc_sockaddr pair. caml_unix_check_path
   has no such macro and is called directly by its prefixed name. */
CAMLextern value caml_win32_alloc_handle(HANDLE);
CAMLextern void caml_win32_maperr(DWORD errcode);
CAMLextern value caml_unix_error_of_code(int errcode);
CAMLnoret CAMLextern void caml_uerror(const char *cmdname, value arg);
CAMLextern void caml_unix_check_path(value path, const char *cmdname);
CAMLextern void caml_unix_get_sockaddr(value mladdr, union sock_addr_union *addr, socklen_param_type *addr_len);
CAMLextern value caml_unix_alloc_sockaddr(union sock_addr_union *addr, socklen_param_type addr_len, int close_on_error);

#define SIZEBUF 4096

struct sock_addr_data {
  union sock_addr_union sock_addr_addr;
  socklen_param_type sock_addr_len;
};

#define Sock_addr_val(v) (*((struct sock_addr_data **) Data_custom_val(v)))

static void finalize_sock_addr(value v) {
  caml_stat_free(Sock_addr_val(v));
  Sock_addr_val(v) = NULL;
}

static struct custom_operations sockaddr_ops = {
  "iocp.sockaddr",
  finalize_sock_addr,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

value
ocaml_iocp_make_sockaddr(value v_sockaddr) {
  CAMLparam1(v_sockaddr);
  CAMLlocal1(v);
  struct sock_addr_data *data;
  v = caml_alloc_custom_mem(&sockaddr_ops, sizeof(struct sock_addr_data *), sizeof(struct sock_addr_data));
  Sock_addr_val(v) = NULL;
  data = (struct sock_addr_data *) caml_stat_alloc(sizeof(struct sock_addr_data));
  Sock_addr_val(v) = data;
  // If this raises, the GC will free [v], which will free [data]:
  get_sockaddr(v_sockaddr, &data->sock_addr_addr, &data->sock_addr_len);
  CAMLreturn(v);
}

value
ocaml_iocp_extract_sockaddr(value v) {
  CAMLparam1(v);
  CAMLlocal1(v_sockaddr);
  struct sock_addr_data *data = Sock_addr_val(v);
  v_sockaddr = alloc_sockaddr(&data->sock_addr_addr, data->sock_addr_len, -1);
  CAMLreturn(v_sockaddr);
}

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
  assert((v & 1) == 1);
  return (LPOVERLAPPED) (v & ~1);
}

void ocaml_iocp_free_overlapped(value v) {
    CAMLparam1(v);
    eo *lp=(eo *)overlapped_ptr_of_val(Field(v,0));
    caml_stat_free(lp);
    CAMLreturn0;
}

value ocaml_iocp_alloc_overlapped(value key) {
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
  /* Split the 63-bit file offset across Offset/OffsetHigh so that positioned
     reads/writes past 4 GB address the correct location (Offset alone is only
     32-bit). */
  ULONG64 o = (ULONG64)(intnat)Long_val(off);
  memset(ol, 0, sizeof(OVERLAPPED));
  ol->Offset = (DWORD)o;
  ol->OffsetHigh = (DWORD)(o >> 32);
  CAMLreturn0;
}

/* Zero the OVERLAPPED body of a recycled structure, preserving the key (which
   lives after the OVERLAPPED in [eo]). Clears any state the previous operation
   left behind when an OVERLAPPED is drawn again from the free pool. */
void ocaml_iocp_reset_overlapped(value v) {
  CAMLparam1(v);
  LPOVERLAPPED ol = overlapped_ptr_of_val(Field(v, 0));
  memset(ol, 0, sizeof(OVERLAPPED));
  CAMLreturn0;
}

value ocaml_iocp_get_overlapped_key(value v) {
  CAMLparam1(v);
  eo *ol = (eo *)overlapped_ptr_of_val(v);
  CAMLreturn(ol->key);
}

void ocaml_iocp_set_overlapped_key(value v, value key) {
  CAMLparam2(v, key);
  /* [v] is the boxed overlapped; field 0 holds the tagged pointer (cf.
     ocaml_iocp_set_overlapped_off). */
  eo *ol = (eo *)overlapped_ptr_of_val(Field(v, 0));
  if(Is_block(key)) {
      caml_invalid_argument("Overlapped key must be an immediate");
  }
  ol->key = key;
  CAMLreturn0;
}

value ocaml_iocp_create_io_completion_port(value v_threads) {
    CAMLparam1(v_threads);
    int num_threads = Int_val(v_threads);

    HANDLE cp = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0 /* ignored */, num_threads);

    if (cp == NULL) {
      win32_maperr(GetLastError());
      uerror("CreateIoCompletionPort", Nothing);
    }

    CAMLreturn(win_alloc_handle(cp));
}

value ocaml_iocp_associate_fd_with_iocp(value v_iocp, value v_fd, value v_key) {
    CAMLparam3(v_iocp, v_fd, v_key);

    HANDLE handle = Handle_val(v_fd);
    HANDLE iocp = Handle_val(v_iocp);

    /* Store the raw integer key so it round-trips through
       GetQueuedCompletionStatus's completion-key out-param unchanged. */
    HANDLE cp = CreateIoCompletionPort(handle, iocp, (ULONG_PTR)Long_val(v_key), 0);

    if (cp == NULL) {
      win32_maperr(GetLastError());
      uerror("CreateIoCompletionPort", Nothing);
    }

    CAMLreturn(v_fd);

}
value ocaml_iocp_get_queued_completion_status(value v_fd, value v_timeout) {
    CAMLparam2(v_fd, v_timeout);
    CAMLlocal2(v,v_err);
    BOOL b = 0;
    HANDLE fd = Handle_val(v_fd);
    DWORD transferred = 0;
    DWORD_PTR ptr = 0;
    DWORD err = 0;
    v_err = Val_int(0); /* None */
    LPOVERLAPPED ol = NULL;

    caml_enter_blocking_section();
    b = GetQueuedCompletionStatus(fd, &transferred, &ptr, &ol, Int_val(v_timeout));
    /* Snapshot the last-error inside the blocking section: caml_leave_blocking_section
       runs async/signal handling that may issue Win32 calls and clobber it. */
    if (!b) err = GetLastError();
    caml_leave_blocking_section();

    /* Indicates an error with the call to GetQueuedCompletionStatus */
    if (!b && ol == NULL) {
      /* A timeout is an OK result, don't raise an exception.
         I have also seen ERROR_SUCCESS, so let's not raise
         an error for this! */
      if(err==WAIT_TIMEOUT || err==ERROR_SUCCESS) {
        CAMLreturn(Val_int(0));
      }

      /* For all other errors, raise a Unix_error */
      win32_maperr(err);
      uerror("QueuedCompletionStatus", Nothing);
    }

    /* Indicates an error with the IO operation represented by ol */
    if(!b && ol != NULL) {
      int e;
      /* Map the captured Win32 error to errno, then snapshot errno into a local
         before caml_alloc (which may run the GC/CRT and overwrite errno). */
      win32_maperr(err);
      e = errno;
      v_err = caml_alloc(1, 0);
      Store_field(v_err, 0, unix_error_of_code(e));
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
    DWORD_PTR ptr = 0;
    DWORD err=0;
    LPOVERLAPPED ol = NULL;


    caml_enter_blocking_section();
    b = GetQueuedCompletionStatus(fd, &transferred, &ptr, &ol, Int_val(v_timeout));
    /* Capture the error inside the blocking section, before leave runs async
       handling that can clobber the last-error. */
    if (!b) err = GetLastError();
    caml_leave_blocking_section();

    Store_field(v, 0, Val_int(ptr));
    Store_field(v, 1, Val_int(transferred));
    Store_field(v, 2, val_of_overlapped_ptr((eo *)ol));
    Store_field(v, 3, Val_bool(b));
    Store_field(v, 4, Val_int(err));

    CAMLreturn0;
}

value ocaml_iocp_read(value v_fd, value v_ba, value v_num_bytes, value v_off, value v_overlapped) {
    CAMLparam5(v_fd, v_ba, v_num_bytes, v_off, v_overlapped);
    LPOVERLAPPED ol = overlapped_ptr_of_val(Field(v_overlapped,0));
    void *buf = Caml_ba_data_val(v_ba) + Long_val(v_off);
    BOOL b = ReadFile(Handle_val(v_fd), buf, Int_val(v_num_bytes), NULL, ol);
    // FALSE with ERROR_IO_PENDING just means the op is running asynchronously.
    if (!b) {
      DWORD err = GetLastError();
      if(err == ERROR_IO_PENDING) CAMLreturn(Val_unit);
      win32_maperr(err);
      uerror("ReadFile", Nothing);
    }
    CAMLreturn(Val_unit);
}

value ocaml_iocp_write(value v_fd, value v_ba, value v_num_bytes, value v_off, value v_overlapped) {
    CAMLparam5(v_fd, v_ba, v_num_bytes, v_off, v_overlapped);
    LPOVERLAPPED ol = overlapped_ptr_of_val(Field(v_overlapped,0));
    void *buf = Caml_ba_data_val(v_ba) + Long_val(v_off);
    BOOL b = WriteFile(Handle_val(v_fd), buf, Int_val(v_num_bytes), NULL, ol);
    if (!b) {
      DWORD err = GetLastError();
      if(err == ERROR_IO_PENDING) CAMLreturn(Val_unit);
      win32_maperr(err);
      uerror("WriteFile", Nothing);
    }
    CAMLreturn(Val_unit);
}

GUID GuidGetAddrAcceptEx = WSAID_GETACCEPTEXSOCKADDRS;

static LPFN_GETACCEPTEXSOCKADDRS get_accept_ex_sockaddrs(SOCKET s) {
    static LPFN_GETACCEPTEXSOCKADDRS fn = NULL;
    if (fn == NULL) {
      DWORD dwBytes;
      if (WSAIoctl(s, SIO_GET_EXTENSION_FUNCTION_POINTER,
                   &GuidGetAddrAcceptEx, sizeof(GuidGetAddrAcceptEx),
                   &fn, sizeof(fn), &dwBytes, NULL, NULL) == SOCKET_ERROR)
        fn = NULL;
    }
    return fn;
}

value ocaml_iocp_accept_buffer_size(value v_unit) {
    CAMLparam1(v_unit);
    CAMLreturn(Val_int(2 * (sizeof(union sock_addr_union) + 16)));
}

value ocaml_iocp_get_accept_ex_sockaddr(value v_accept_buffer, value v_listen, value v_sockaddr) {
    CAMLparam3(v_accept_buffer, v_listen, v_sockaddr);
    int lsize = 0;
    SOCKADDR *pLocal = NULL, *pRemote = NULL;
    struct sock_addr_data *data = Sock_addr_val(v_sockaddr);

    LPFN_GETACCEPTEXSOCKADDRS lpfnGetAcceptExSockaddrs =
        get_accept_ex_sockaddrs(Socket_val(v_listen));
    if (lpfnGetAcceptExSockaddrs == NULL) {
        win32_maperr(WSAGetLastError());
        uerror("WSAIoctl", Nothing);
    }

    lpfnGetAcceptExSockaddrs(
        Caml_ba_data_val(v_accept_buffer),
        0,
        sizeof(union sock_addr_union) + 16,
        sizeof(union sock_addr_union) + 16,
        &pLocal,
        &lsize,
        &pRemote,
        &(data->sock_addr_len));

    memcpy(&(data->sock_addr_addr), pRemote, data->sock_addr_len);

    CAMLreturn(Val_unit);
}

GUID GuidAcceptEx = WSAID_ACCEPTEX;

static LPFN_ACCEPTEX get_accept_ex(SOCKET s) {
    static LPFN_ACCEPTEX fn = NULL;
    if (fn == NULL) {
      DWORD dwBytes;
      if (WSAIoctl(s, SIO_GET_EXTENSION_FUNCTION_POINTER,
                   &GuidAcceptEx, sizeof(GuidAcceptEx),
                   &fn, sizeof(fn), &dwBytes, NULL, NULL) == SOCKET_ERROR)
        fn = NULL;
    }
    return fn;
}

void ocaml_iocp_accept(value v_listen, value v_accept, value v_accept_buffer, value v_overlapped) {
    CAMLparam4(v_listen, v_accept, v_accept_buffer, v_overlapped);
    LPOVERLAPPED ol = overlapped_ptr_of_val(Field(v_overlapped,0));
    DWORD received;

    LPFN_ACCEPTEX lpfnAcceptEx = get_accept_ex(Socket_val(v_listen));

    if (lpfnAcceptEx == NULL) {
      win32_maperr(WSAGetLastError());
      uerror("WSAIoctl", Nothing);
    }

    BOOL b = lpfnAcceptEx(
        Socket_val(v_listen),               // The listening socket
        Socket_val(v_accept),               // A fresh, unbound, unconnected socket of the listener's type (AcceptEx requirement)
        Caml_ba_data_val(v_accept_buffer),  // An output buffer that receives the first block of data (and sockaddr)
        0,                                  // Receive data length, the number of bytes in the buffer         
        sizeof(union sock_addr_union) + 16, // Number of bytes reserved for local address information
        sizeof(union sock_addr_union) + 16, // Number of bytes reserved for remote address information
        &received,                          // Pointer to DWORD for number of bytes received
        ol);                                // The OVERLAPPED structure

    if (!b) {
      DWORD err = WSAGetLastError();
      if(err == ERROR_IO_PENDING) {
        CAMLreturn0;
      }
      win32_maperr(err);
      uerror("AcceptEx", Nothing);
    }
    CAMLreturn0;
}

GUID GuidConnectEx = WSAID_CONNECTEX;

static LPFN_CONNECTEX get_connect_ex(SOCKET s) {
    static LPFN_CONNECTEX fn = NULL;
    if (fn == NULL) {
      DWORD dwBytes;
      if (WSAIoctl(s, SIO_GET_EXTENSION_FUNCTION_POINTER,
                   &GuidConnectEx, sizeof(GuidConnectEx),
                   &fn, sizeof(fn), &dwBytes, NULL, NULL) == SOCKET_ERROR)
        fn = NULL;
    }
    return fn;
}

void ocaml_iocp_connect(value v_sock, value v_addr, value v_overlapped) {
    CAMLparam3(v_sock, v_addr, v_overlapped);
    LPOVERLAPPED ol = overlapped_ptr_of_val(Field(v_overlapped,0));
    DWORD received;

    LPFN_CONNECTEX lpfnConnectEx = get_connect_ex(Socket_val(v_sock));

    if (lpfnConnectEx == NULL) {
        win32_maperr(WSAGetLastError());
        uerror("WSAIoctl", Nothing);
    }

    struct sock_addr_data *data = Sock_addr_val(v_addr);

    BOOL b = lpfnConnectEx(
        Socket_val(v_sock),                 // The socket
        &(data->sock_addr_addr.s_gen),      // The peer address to connect to
        data->sock_addr_len,                // namelen: length of the sockaddr
        NULL,                               // lpSendBuffer: no data sent on connect
        0,                                  // dwSendDataLength
        &received,                          // lpdwBytesSent
        ol);                                // The OVERLAPPED structure

    // The return value is non-zero (TRUE) on success. However, it is FALSE if the IO operation
    // is completing asynchronously. We change that behaviour by checking last error.
    if (!b) {
      DWORD err = WSAGetLastError();
      if(err == ERROR_IO_PENDING) {
        CAMLreturn0;
      }
      win32_maperr(err);
      uerror("ConnectEx", Nothing);
    }
    CAMLreturn0;
}

// WSABUF

#define Wsabuf_val(v) (*((WSABUF **) Data_custom_val(v)))

static void finalize_wsabuf(value v) {
  caml_stat_free(Wsabuf_val(v));
  Wsabuf_val(v) = NULL;
}

static struct custom_operations wsabuf_ops = {
  "iocp.wsabuf_ops",
  finalize_wsabuf,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

value
ocaml_iocp_make_wsabuf(value v_cstructs, value v_len) {
  CAMLparam2(v_cstructs, v_len);
  CAMLlocal2(v, l);
  int len = Int_val(v_len);
  int i;
  WSABUF *bufs;
  // Allocate the custom block on the OCaml heap:
  v = caml_alloc_custom_mem(&wsabuf_ops, sizeof(WSABUF *), len * sizeof(WSABUF));
  Wsabuf_val(v) = NULL;
  bufs = caml_stat_alloc(len * sizeof(WSABUF));
  Wsabuf_val(v) = bufs;
  for (i = 0, l = v_cstructs; i < len; l = Field(l, 1), i++) {
    value v_cs = Field(l, 0);
    value v_ba = Field(v_cs, 0);
    value v_off = Field(v_cs, 1);
    value v_cs_len = Field(v_cs, 2);
    bufs[i].buf = Caml_ba_data_val(v_ba) + Long_val(v_off);
    bufs[i].len = Long_val(v_cs_len);
  }
  CAMLreturn(v);
}

void
ocaml_iocp_send(value v_sock, value v_wsabuf, value v_overlapped) {
  CAMLparam3(v_sock, v_wsabuf, v_overlapped);
  DWORD received;
  LPOVERLAPPED ol = overlapped_ptr_of_val(Field(v_overlapped,0));

  WSABUF *wsabuf = Wsabuf_val(Field(v_wsabuf, 0));
  int len = Int_val(Field(v_wsabuf, 1));

  int i = WSASend(
    Socket_val(v_sock),
    wsabuf,
    len,
    &received,
    0,
    ol,
    NULL
  );

  if (i==SOCKET_ERROR) {
      DWORD err = WSAGetLastError();
      if(err == ERROR_IO_PENDING) {
        CAMLreturn0;
      }
      win32_maperr(err);
      uerror("WSASend", Nothing);
    }
    CAMLreturn0;
}

void
ocaml_iocp_recv(value v_sock, value v_wsabuf, value v_overlapped) {
  CAMLparam3(v_sock, v_wsabuf, v_overlapped);
  DWORD received, flags;
  LPOVERLAPPED ol = overlapped_ptr_of_val(Field(v_overlapped,0));

  WSABUF *wsabuf = Wsabuf_val(Field(v_wsabuf, 0));
  int len = Int_val(Field(v_wsabuf, 1));


  flags = 0;

  int i = WSARecv(
    Socket_val(v_sock),
    wsabuf,
    len,
    &received,
    &flags,
    ol,
    NULL
  );

  if (i==SOCKET_ERROR) {
      DWORD err = WSAGetLastError();
      if(err == ERROR_IO_PENDING) {
        CAMLreturn0;
      }
      win32_maperr(err);
      uerror("WSARecv", Nothing);
    }
    CAMLreturn0;
}

/* SO_UPDATE_ACCEPT_CONTEXT takes the *listening* socket as its option value. */
value ocaml_iocp_update_accept_ctx(value v_accept, value v_listen) {
  CAMLparam2(v_accept, v_listen);
  SOCKET listen_sock = Socket_val(v_listen);
  if (setsockopt(Socket_val(v_accept), SOL_SOCKET, SO_UPDATE_ACCEPT_CONTEXT,
                 (char *) &listen_sock, sizeof(listen_sock)) == SOCKET_ERROR) {
    win32_maperr(WSAGetLastError());
    uerror("setsockopt", Nothing);
  }
  CAMLreturn(Val_unit);
}

/* SO_UPDATE_CONNECT_CONTEXT takes no option value. */
value ocaml_iocp_update_connect_ctx(value v_sock) {
  CAMLparam1(v_sock);
  if (setsockopt(Socket_val(v_sock), SOL_SOCKET, SO_UPDATE_CONNECT_CONTEXT,
                 NULL, 0) == SOCKET_ERROR) {
    win32_maperr(WSAGetLastError());
    uerror("setsockopt", Nothing);
  }
  CAMLreturn(Val_unit);
}

value ocaml_iocp_cancel(value v_fd, value v_overlapped) {
  CAMLparam2(v_fd, v_overlapped);
  LPOVERLAPPED ol = overlapped_ptr_of_val(Field(v_overlapped,0));

  /* The cancelled operation still posts an ERROR_OPERATION_ABORTED completion,
     which the caller must drain. ERROR_NOT_FOUND just means it already
     completed, which is not an error here. */
  if (!CancelIoEx(Handle_val(v_fd), ol)) {
    DWORD err = GetLastError();
    if (err != ERROR_NOT_FOUND) {
      win32_maperr(err);
      uerror("CancelIoEx", Nothing);
    }
  }
  CAMLreturn(Val_unit);
}

value ocaml_iocp_cancel_all(value v_fd) {
  CAMLparam1(v_fd);

  /* Cancel every outstanding overlapped operation on this handle. Each still
     posts an ERROR_OPERATION_ABORTED completion that the caller must drain.
     ERROR_NOT_FOUND just means nothing was pending, which is fine here. */
  if (!CancelIoEx(Handle_val(v_fd), NULL)) {
    DWORD err = GetLastError();
    if (err != ERROR_NOT_FOUND) {
      win32_maperr(err);
      uerror("CancelIoEx", Nothing);
    }
  }
  CAMLreturn(Val_unit);
}

value ocaml_iocp_post(value v_iocp, value v_key, value v_bytes) {
  CAMLparam3(v_iocp, v_key, v_bytes);
  BOOL b = PostQueuedCompletionStatus(Handle_val(v_iocp),
                                      (DWORD)Long_val(v_bytes),
                                      (ULONG_PTR)Long_val(v_key),
                                      NULL);
  if (!b) {
    win32_maperr(GetLastError());
    uerror("PostQueuedCompletionStatus", Nothing);
  }
  CAMLreturn(Val_unit);
}

typedef struct iocp_wait_ctx {
  HANDLE iocp;
  ULONG_PTR key;
  HANDLE wait;
} iocp_wait_ctx;

static VOID CALLBACK iocp_wait_callback(PVOID param, BOOLEAN timedOut) {
  iocp_wait_ctx *ctx = (iocp_wait_ctx *)param;
  (void)timedOut;
  PostQueuedCompletionStatus(ctx->iocp, 0, ctx->key, NULL);
}

value ocaml_iocp_register_wait(value v_iocp, value v_handle, value v_key) {
  CAMLparam3(v_iocp, v_handle, v_key);
  iocp_wait_ctx *ctx = caml_stat_alloc(sizeof(iocp_wait_ctx));
  ctx->iocp = Handle_val(v_iocp);
  ctx->key = (ULONG_PTR)Long_val(v_key);
  ctx->wait = NULL;
  if (!RegisterWaitForSingleObject(&ctx->wait, Handle_val(v_handle),
                                   iocp_wait_callback, ctx,
                                   INFINITE, WT_EXECUTEONLYONCE)) {
    DWORD err = GetLastError();
    caml_stat_free(ctx);
    win32_maperr(err);
    uerror("RegisterWaitForSingleObject", Nothing);
  }
  CAMLreturn(caml_copy_nativeint((intnat)ctx));
}

value ocaml_iocp_unregister_wait(value v_ctx) {
  CAMLparam1(v_ctx);
  iocp_wait_ctx *ctx = (iocp_wait_ctx *)Nativeint_val(v_ctx);
  if (ctx == NULL) CAMLreturn(Val_unit);   /* already unregistered (defensive) */
  caml_enter_blocking_section();
  UnregisterWaitEx(ctx->wait, INVALID_HANDLE_VALUE);
  caml_leave_blocking_section();
  caml_stat_free(ctx);
  CAMLreturn(Val_unit);
}

void
ocaml_iocp_recv_from(value v_sock, value v_wsabuf, value v_sockaddr, value v_overlapped) {
  CAMLparam4(v_sock, v_wsabuf, v_sockaddr, v_overlapped);
  DWORD received, flags = 0;
  LPOVERLAPPED ol = overlapped_ptr_of_val(Field(v_overlapped,0));
  WSABUF *wsabuf = Wsabuf_val(Field(v_wsabuf, 0));
  int len = Int_val(Field(v_wsabuf, 1));
  struct sock_addr_data *data = Sock_addr_val(v_sockaddr);

  /* [sock_addr_len] is in/out: initialise it to the buffer capacity. */
  data->sock_addr_len = sizeof(union sock_addr_union);

  int i = WSARecvFrom(Socket_val(v_sock), wsabuf, len, &received, &flags,
                      &data->sock_addr_addr.s_gen, &data->sock_addr_len, ol, NULL);
  if (i == SOCKET_ERROR) {
    DWORD err = WSAGetLastError();
    if (err == ERROR_IO_PENDING) CAMLreturn0;
    win32_maperr(err);
    uerror("WSARecvFrom", Nothing);
  }
  CAMLreturn0;
}

void
ocaml_iocp_send_to(value v_sock, value v_wsabuf, value v_sockaddr, value v_overlapped) {
  CAMLparam4(v_sock, v_wsabuf, v_sockaddr, v_overlapped);
  DWORD sent;
  LPOVERLAPPED ol = overlapped_ptr_of_val(Field(v_overlapped,0));
  WSABUF *wsabuf = Wsabuf_val(Field(v_wsabuf, 0));
  int len = Int_val(Field(v_wsabuf, 1));
  struct sock_addr_data *data = Sock_addr_val(v_sockaddr);

  int i = WSASendTo(Socket_val(v_sock), wsabuf, len, &sent, 0,
                    &data->sock_addr_addr.s_gen, data->sock_addr_len, ol, NULL);
  if (i == SOCKET_ERROR) {
    DWORD err = WSAGetLastError();
    if (err == ERROR_IO_PENDING) CAMLreturn0;
    win32_maperr(err);
    uerror("WSASendTo", Nothing);
  }
  CAMLreturn0;
}

static int shutdown_command_table[] = {
  0, 1, 2
};

value ocaml_iocp_shutdown(value v_sock, value cmd)
{
  CAMLparam2(v_sock, cmd);
  if (shutdown(Socket_val(v_sock), shutdown_command_table[Int_val(cmd)]) == -1) {
    win32_maperr(WSAGetLastError());
    uerror("shutdown", Nothing);
  }
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

/* A completion-port-aware replacement for Unix.openfile.
 *
 * The standard library always opens handles synchronously, but every handle
 * that we hand to an IOCP has to be created with FILE_FLAG_OVERLAPPED. So we
 * call CreateFileW with the overlapped attribute set, and register the result.
 */
enum open_flag {
  FLAG_RDONLY = 0, FLAG_WRONLY, FLAG_RDWR,
  FLAG_NONBLOCK, FLAG_APPEND,
  FLAG_CREAT, FLAG_TRUNC, FLAG_EXCL,
  FLAG_NOCTTY, FLAG_DSYNC, FLAG_SYNC, FLAG_RSYNC,
  FLAG_SHARE_DELETE, FLAG_CLOEXEC, FLAG_KEEPEXEC
};

value ocaml_iocp_unix_pipe(value v_iocp, value v_id1, value v_id2, value v_path)
{
  CAMLparam4(v_iocp, v_id1, v_id2, v_path);
  CAMLlocal2(readfd, writefd);
  value res;
  wchar_t *wpath = caml_stat_strdup_to_utf16(String_val(v_path));
  HANDLE iocp = Handle_val(v_iocp);

  SECURITY_ATTRIBUTES attr;
  attr.nLength = sizeof(attr);
  attr.lpSecurityDescriptor = NULL;
  attr.bInheritHandle = TRUE;

  HANDLE pipeR = CreateNamedPipeW(
        wpath, // name of the pipe
        (PIPE_ACCESS_DUPLEX | FILE_FLAG_OVERLAPPED),
        (PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT),
        1, // only allow 1 instance of this pipe
        SIZEBUF,
        SIZEBUF,
        PIPE_WAIT, // use default wait time
        &attr // use default security attributes
    );

  if (pipeR == INVALID_HANDLE_VALUE || pipeR == NULL) {
    win32_maperr(GetLastError());
    caml_stat_free(wpath);
    uerror("CreateNamedPipe", Nothing);
  }

  HANDLE pipeW = CreateFileW(
        wpath,
        GENERIC_READ | GENERIC_WRITE,
        FILE_SHARE_READ | FILE_SHARE_WRITE,
        NULL,
        OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL | FILE_FLAG_OVERLAPPED,
        &attr);

  caml_stat_free(wpath);  /* freed only now: used by both calls above */

  if (pipeW == INVALID_HANDLE_VALUE || pipeW == NULL) {
    win32_maperr(GetLastError());
    CloseHandle(pipeR);
    uerror("CreateFile", Nothing);
  }

  HANDLE t = CreateIoCompletionPort(pipeR, iocp, (ULONG_PTR)Long_val(v_id1), 0);

  if (t == NULL) {
      win32_maperr(GetLastError());
      CloseHandle(pipeR);
      CloseHandle(pipeW);
      uerror("CreateIoCompletionPort", Nothing);
    }

  t = CreateIoCompletionPort(pipeW, iocp, (ULONG_PTR)Long_val(v_id2), 0);

  if (t == NULL) {
      win32_maperr(GetLastError());
      CloseHandle(pipeR);
      CloseHandle(pipeW);
      uerror("CreateIoCompletionPort", Nothing);
    }

  writefd = win_alloc_handle(pipeW);
  readfd = win_alloc_handle(pipeR);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = readfd;
  Field(res, 1) = writefd;
  CAMLreturn(res);
}

value ocaml_iocp_unix_open(value v_iocp, value v_id, value path, value flags, value perm)
{
  CAMLparam5(v_iocp, v_id, path, flags, perm);
  HANDLE port = Handle_val(v_iocp);
  HANDLE handle;
  SECURITY_ATTRIBUTES sec;
  wchar_t *wide_path;
  DWORD access = 0;
  DWORD sharing = FILE_SHARE_READ | FILE_SHARE_WRITE;
  DWORD disposition;
  BOOL want_create = FALSE, want_trunc = FALSE, want_excl = FALSE;
  BOOL no_inherit = FALSE, keep_inherit = FALSE;
  value cell;

  caml_unix_check_path(path, "open");

  for (cell = flags; cell != Val_emptylist; cell = Field(cell, 1)) {
    switch (Int_val(Field(cell, 0))) {
      case FLAG_RDONLY:       access |= GENERIC_READ; break;
      case FLAG_WRONLY:       access |= GENERIC_WRITE; break;
      case FLAG_RDWR:         access |= GENERIC_READ | GENERIC_WRITE; break;
      case FLAG_CREAT:        want_create = TRUE; break;
      case FLAG_TRUNC:        want_trunc = TRUE; break;
      case FLAG_EXCL:         want_excl = TRUE; break;
      case FLAG_SHARE_DELETE: sharing |= FILE_SHARE_DELETE; break;
      case FLAG_CLOEXEC:      no_inherit = TRUE; break;
      case FLAG_KEEPEXEC:     keep_inherit = TRUE; break;
      default:                break;  /* flags with no Win32 equivalent */
    }
  }

  if (want_create) {
    if (want_excl)
      disposition = CREATE_NEW;
    else if (want_trunc)
      disposition = CREATE_ALWAYS;
    else
      disposition = OPEN_ALWAYS;
  } else if (want_trunc) {
    disposition = TRUNCATE_EXISTING;
  } else {
    disposition = OPEN_EXISTING;
  }

  sec.nLength = sizeof(sec);
  sec.lpSecurityDescriptor = NULL;
  if (no_inherit)
    sec.bInheritHandle = FALSE;
  else if (keep_inherit)
    sec.bInheritHandle = TRUE;
  else
    sec.bInheritHandle = !unix_cloexec_default;

  wide_path = caml_stat_strdup_to_utf16(String_val(path));
  handle = CreateFileW(wide_path, access, sharing, &sec,
                       disposition, FILE_FLAG_OVERLAPPED, NULL);
  caml_stat_free(wide_path);

  if (handle == INVALID_HANDLE_VALUE) {
    win32_maperr(GetLastError());
    uerror("open", path);
  }

  if (CreateIoCompletionPort(handle, port, (ULONG_PTR)Long_val(v_id), 0) == NULL) {
    win32_maperr(GetLastError());
    CloseHandle(handle);
    uerror("CreateIoCompletionPort", Nothing);
  }

  CAMLreturn(win_alloc_handle(handle));
}
