# ocaml-iocp

Windows 10 and later I/O completion ports for OCaml 5.2 and later.

The library provides file and pipe reads/writes, cancellation, and cross-thread notifications. Request IDs
are ordinary positive integers. Buffers and request storage remain rooted until
completion is consumed, including when cancellation races with successful I/O.
There is no fixed limit on the number of outstanding requests.

## Ownership

- Use one completion consumer per port. Submission and cancellation are thread-safe.
- Call `Iocp.register port fd` before submitting I/O. The returned handle borrows
  the descriptor; keep it open and unchanged until `Iocp.close_handle` returns.
- Aliases can register with the same port. Registering an alias with another port
  fails while any registration remains open. Closing the last registration
  detaches the object, allowing it to be registered again later.
- Descriptors must support overlapped I/O. Do not change their completion
  notification modes or issue overlapped I/O outside this library.
- Cancellation requests completion; it does not release the request's buffers.
- Drain completions before closing registrations, then close the port. Both
  operations reject resources that are still busy.
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
  ignore (Iocp.read port ~id:1 handle buf ~offset:0L);
  match Iocp.wait port ~timeout:(-1) with
  | Some { error = None; bytes_transferred; _ } ->
      Cstruct.to_string ~len:bytes_transferred buf
  | Some { error = Some e; _ } -> raise (Unix.Unix_error (e, "read", path))
  | None -> assert false
```

`Iocp.pipe` creates private byte pipes with non-inheritable handles. Choose
synchronous ends explicitly when handing them to programs that use synchronous
standard I/O. Files normally use `Unix.openfile` followed by `ReOpenFile`. When creating a
read-only file with write access, a single overlapped open sets the attribute
at creation and preserves the creator's write access.
Offsets are 64-bit, and append mode does not grant additional write access.

## Build and test

```
opam install dune cstruct alcotest
dune build @install
dune runtest
```

The suite runs in native and self-contained bytecode modes, so both use the
current stubs rather than an older installed DLL. It exercises completion delivery,
cancellation and GC, failed submission
rollback, large request counts, file offsets, Unicode paths and private pipes. Tests use temporary files and ephemeral
ports. The earlier unreleased prototype API has been replaced; raw pointers,
manual OVERLAPPED reuse, and the fixed-size request pool are no longer exposed.
