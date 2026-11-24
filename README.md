# ocaml-iocp

Windows 10 and later I/O completion ports for OCaml 5.2 and later.

The library provides file and pipe reads/writes, socket and datagram send/receive,
AcceptEx, ConnectEx, cancellation, and cross-thread notifications. Each submission carries an
OCaml value returned with its queued completion. Buffers and request storage remain rooted until
completion is consumed, including when cancellation races with successful I/O.
There is no fixed limit on the number of outstanding requests.

## Ownership

- Multiple threads may submit, cancel and consume completions on a port.
- `Iocp.Event` accepts an overlapped descriptor directly. Each request uses its
  own event and preserves the descriptor's completion-port settings. Keep the
  descriptor open and unchanged until the request's completion is consumed.
- For a persistent association, call `Iocp.register port fd` and keep the
  descriptor open and unchanged until `Iocp.close_handle` returns.
- Aliases can register with the same port. Registering an alias with another port
  fails while any registration remains open. Closing the last registration
  detaches the object, allowing it to be registered again later.
- Submit a registered handle on the port where its consumer waits. When the
  ports differ, Windows delivers the operation to the submitting port, even
  after the thread that registered the handle exits.
- Registered descriptors must support overlapped I/O. Keep their notification
  modes unchanged and submit overlapped I/O through this library.
- Cancellation requests completion; it does not release the request's buffers.
- Drain completions before closing registrations, then close the port. Both
  operations reject resources that are still busy.
- Use `Iocp.shutdown` for local socket receive shutdown: Windows does not reliably
  complete an outstanding receive when `Unix.shutdown` is used directly.
  For event-based receives, call `Iocp.Event.shutdown` and then
  `Iocp.Event.cancel_receives` on each submitting port.
- Explicit cleanup is required. Abandoning a pending request retains its resources
  rather than allowing Windows to access memory reclaimed by the GC.

## Example

```ocaml
let read_file path =
  let fd = Iocp.openfile path [Unix.O_RDONLY; O_CLOEXEC] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
  let port = Iocp.create () in
  Fun.protect ~finally:(fun () -> Iocp.close port) @@ fun () ->
  let handle = Iocp.register port fd in
  Fun.protect ~finally:(fun () -> Iocp.close_handle handle) @@ fun () ->
  let buf = Cstruct.create 4096 in
  ignore (Iocp.read port ~data:() handle buf ~offset:0L);
  let rec receive () = match Iocp.wait port ~timeout:(-1) with
  | Some (Iocp.Completion ((), { error = None; bytes_transferred; _ })) ->
      Cstruct.to_string ~len:bytes_transferred buf
  | Some (Completion ((), { error = Some e; _ })) -> raise (Unix.Unix_error (e, "read", path))
  | Some Wake | None -> receive ()
  in
  receive ()
```

`Iocp.pipe` creates private byte pipes with non-inheritable handles. Choose
synchronous ends explicitly when handing them to programs that use synchronous
standard I/O. Files normally use `Unix.openfile` followed by `ReOpenFile`. When creating a
read-only file with write access, a single overlapped open sets the attribute
at creation and preserves the creator's write access.
Offsets are 64-bit, and append mode does not grant additional write access.

`Iocp.create ~skip_on_success:true ()` avoids a completion-port round trip when
Windows completes an operation immediately. In this mode, check `Iocp.result`
after each submission: `Some completion` is already finished; `None` must be
drained with `Iocp.wait`. Unsupported providers retain queued delivery. The
default mode continues to deliver every completion through `wait`.

## Build and test

```
opam install dune cstruct alcotest
dune build @install
dune runtest
```

The suite runs in native, self-contained bytecode and dynamically loaded
bytecode modes. It exercises completion delivery,
cancellation and GC, failed submission
rollback, large request counts, file offsets, Unicode paths, private pipes,
socket connection errors, and shutdown. Tests use temporary files and ephemeral
ports. The earlier unreleased prototype API has been replaced; raw pointers,
manual OVERLAPPED reuse, and the fixed-size request pool are no longer exposed.
