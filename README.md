# ocaml-iocp -- bindings to Windows IOCP

These are OCaml bindings for Windows' [input/output completion ports](https://docs.microsoft.com/en-us/windows/win32/fileio/i-o-completion-ports).

The API is modelled on [ocaml-uring](https://github.com/ocaml-multicore/ocaml-uring). Each
asynchronous operation is submitted against a completion port and identified by an opaque
`Iocp.Id.t`, and completions are reaped from the port and matched back to their submission.

## Usage

Create a completion port, then open files with `Iocp.openfile` or associate an
existing overlapped handle/socket with `Iocp.handle_of_fd`. Submitting an
operation returns an `Iocp.Id.t option`; `None` is if the (bounded) operation
pool is momentarily full. Reap completions with `Iocp.wait`, matching the
returned `id` against the one you were given.

This program reads a block from one file and writes it to another (see `test/copy_lib.ml`
for a full queued-depth file copy):

```ocaml
let () =
  let iocp = Iocp.create 1 in
  let src = Iocp.openfile iocp 1 Sys.argv.(1) [ O_RDONLY ] 0 in
  let dst = Iocp.openfile iocp 2 Sys.argv.(2) [ O_WRONLY; O_CREAT; O_TRUNC ] 0o644 in
  let buf = Cstruct.create 4096 in
  let off = Optint.Int63.zero in
  let read_id = Option.get (Iocp.read iocp src (Cstruct.to_bigarray buf) ~pos:0 ~len:4096 ~off) in
  (match Iocp.wait iocp ~timeout:(-1) with
   | Some (Iocp.Io { id; bytes_transferred; error = None }) ->
     assert (Iocp.Id.equal id read_id);
     let _ = Iocp.write iocp dst (Cstruct.to_bigarray buf) ~pos:0 ~len:bytes_transferred ~off in
     ignore (Iocp.wait iocp ~timeout:(-1))
   | _ -> assert false);
  Unix.close (Iocp.Handle.fd src);
  Unix.close (Iocp.Handle.fd dst)
```
