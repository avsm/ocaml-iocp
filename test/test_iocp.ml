(* Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>
   SPDX-License-Identifier: MIT *)

let registrations_lock = Mutex.create ()
let registrations = ref []

let handle p fd =
  let h = Iocp.register p fd in
  Mutex.protect registrations_lock (fun () ->
      registrations := (p, fd, h) :: !registrations);
  h

let release_matching predicate =
  let selected =
    Mutex.protect registrations_lock (fun () ->
        let selected, remaining = List.partition predicate !registrations in
        registrations := remaining;
        selected)
  in
  List.iter (fun (_, _, h) -> Iocp.close_handle h) selected

let close_fd fd =
  release_matching (fun (_, original, _) -> original == fd);
  Unix.close fd

let close_port p =
  release_matching (fun (owner, _, _) -> owner == p);
  Iocp.close p

let associate p fd = ignore (handle p fd)
let check = Alcotest.(check int)

let status p =
  match Iocp.wait p ~timeout:10000 with
  | Some x -> x
  | None -> Alcotest.fail "completion timed out"

let success p id bytes =
  let c = status p in
  check "id" id c.id;
  check "bytes" bytes c.bytes_transferred;
  match c.error with
  | None -> ()
  | Some e -> Alcotest.fail (Unix.error_message e)

let with_port fn =
  let p = Iocp.create () in
  Fun.protect ~finally:(fun () -> close_port p) (fun () -> fn p)

let with_pipe fn =
  let r, w = Iocp.pipe () in
  Fun.protect
    ~finally:(fun () ->
      close_fd r;
      close_fd w)
    (fun () -> with_port (fun p -> fn p r w))

let invalid fn =
  match fn () with
  | () -> Alcotest.fail "expected Invalid_argument"
  | exception Invalid_argument _ -> ()

let wake () =
  with_port @@ fun p ->
  assert (Iocp.wait p ~timeout:0 = None);
  let d = Domain.spawn (fun () -> Iocp.wake p) in
  success p 0 0;
  Domain.join d

let consumer_handover () =
  let run wake wait =
    let gate = Atomic.make false in
    let finished = Atomic.make false in
    let worker =
      Domain.spawn (fun () ->
          while not (Atomic.get gate) do
            Domain.cpu_relax ()
          done;
          let result = wait 100 in
          Atomic.set finished true;
          result)
    in
    wake ();
    assert (wait 100);
    wake ();
    Atomic.set gate true;
    while not (Atomic.get finished) do
      Domain.cpu_relax ()
    done;
    assert (Domain.join worker);
    assert (not (wait 0))
  in
  with_port (fun p ->
      run (fun () -> Iocp.wake p) (fun timeout -> Iocp.wait p ~timeout <> None))

let closed_port () =
  let p = Iocp.create () in
  close_port p;
  close_port p;
  invalid (fun () -> Iocp.wake p);
  invalid (fun () -> ignore (Iocp.wait p ~timeout:0))

let failed_registration () =
  let sync, writer = Iocp.pipe ~read_async:false () in
  Fun.protect ~finally:(fun () ->
      Unix.close sync;
      Unix.close writer)
  @@ fun () ->
  with_port @@ fun p ->
  (match Iocp.register p sync with
  | _ -> Alcotest.fail "registered a synchronous pipe for overlapped I/O"
  | exception Unix.Unix_error _ -> ());
  Iocp.close_handle (Iocp.register p writer)

let association_ownership () =
  with_pipe @@ fun p r _w ->
  associate p r;
  associate p r;
  with_port @@ fun other ->
  match associate other r with
  | () -> Alcotest.fail "associated a handle with two ports"
  | exception Unix.Unix_error (EINVAL, _, _) -> ()

let with_registration p fd fn =
  let h = Iocp.register p fd in
  Fun.protect ~finally:(fun () -> Iocp.close_handle h) (fun () -> fn h)

let foreign_alias_reuse () =
  with_pipe @@ fun p target _ ->
  with_pipe @@ fun q source _ ->
  with_registration p target @@ fun old ->
  Iocp.close_handle old;
  with_registration q source @@ fun _ ->
  Unix.dup2 source target;
  Unix.dup2 source target;
  match Iocp.register p target with
  | h ->
      Iocp.close_handle h;
      Alcotest.fail "accepted a foreign alias"
  | exception Unix.Unix_error (EINVAL, _, _) -> ()

let delete_sharing () =
  let run perm share =
    let path = Filename.temp_file "iocp-sharing" ".tmp" in
    Sys.remove path;
    let moved = path ^ ".moved" in
    Fun.protect ~finally:(fun () ->
        List.iter (fun p -> if Sys.file_exists p then (Unix.chmod p 0o600; Sys.remove p)) [path; moved])
    @@ fun () ->
    let flags = [Unix.O_WRONLY; O_CREAT; O_EXCL] in
    let fd = Iocp.openfile path (if share then O_SHARE_DELETE :: flags else flags) perm in
    Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
    match Unix.rename path moved with
    | () -> if not share then Alcotest.fail "rename succeeded without delete sharing"
    | exception Unix.Unix_error (EACCES, _, _) when not share -> ()
  in
  List.iter (fun perm -> List.iter (run perm) [false; true]) [0o600; 0o400]

let pipe_modes () =
  List.iter (fun message ->
      let r, w = Iocp.pipe ~read_async:false ~write_async:false ~message () in
      Fun.protect ~finally:(fun () -> Unix.close r; Unix.close w) @@ fun () ->
      let buf = Bytes.create 1 in
      assert (Unix.write_substring w "x" 0 1 = 1);
      assert (Unix.read r buf 0 1 = 1 && Bytes.get buf 0 = 'x')) [false; true]

let creation_permissions () =
  let run mode (exists, was_readonly, flags) =
    let reference = Filename.temp_file "unix-perm" ".tmp" in
    let path = Filename.temp_file "iocp-perm" ".tmp" in
    Fun.protect ~finally:(fun () ->
        List.iter
          (fun p ->
            if Sys.file_exists p then (
              Unix.chmod p 0o600;
              Sys.remove p))
          [ reference; path ])
    @@ fun () ->
    List.iter
      (fun p ->
        if not exists then Sys.remove p
        else Unix.chmod p (if was_readonly then 0o400 else 0o600))
      [ reference; path ];
    let flags = mode :: Unix.O_CLOEXEC :: flags in
    let open_result fn path =
      try Ok (fn path flags 0o400) with Unix.Unix_error (e, _, _) -> Error e
    in
    match open_result Unix.openfile reference with
    | Error expected -> (
        match open_result Iocp.openfile path with
        | Error actual -> assert (expected = actual)
        | Ok fd ->
            close_fd fd;
            Alcotest.fail "Iocp.openfile succeeded where Unix.openfile failed")
    | Ok reference_fd ->
        Fun.protect ~finally:(fun () -> close_fd reference_fd) @@ fun () ->
        let fd = Iocp.openfile path flags 0o400 in
        Fun.protect ~finally:(fun () -> close_fd fd) @@ fun () ->
        check "permissions follow Unix.openfile"
          (Unix.fstat reference_fd).st_perm (Unix.fstat fd).st_perm
  in
  List.iter
    (fun mode ->
      List.iter (run mode)
        [
          (false, false, [ Unix.O_CREAT; O_EXCL ]);
          (false, false, [ O_CREAT ]);
          (false, false, [ O_CREAT; O_TRUNC ]);
          (false, false, [ O_CREAT; O_APPEND ]);
          (false, false, [ O_CREAT; O_APPEND; O_TRUNC ]);
          (true, false, [ O_CREAT ]);
          (true, false, [ O_CREAT; O_TRUNC ]);
          (true, false, [ O_CREAT; O_EXCL ]);
          (true, false, [ O_CREAT; O_APPEND ]);
          (true, false, [ O_CREAT; O_APPEND; O_TRUNC ]);
          (true, true, [ O_CREAT ]);
          (true, true, [ O_CREAT; O_TRUNC ]);
        ])
    [ Unix.O_RDONLY; O_WRONLY; O_RDWR ]

(* Model a failure while converting a dequeued packet, before delivery commits. *)
let retry_delivery () =
  let p = Raw.create_port () in
  Fun.protect ~finally:(fun () -> Raw.close_port p) @@ fun () ->
  let rd, wr = Iocp.pipe () in
  Fun.protect ~finally:(fun () ->
      Unix.close rd;
      Unix.close wr)
  @@ fun () ->
  Raw.associate_port p wr;
  Fun.protect ~finally:(fun () -> Raw.detach wr) @@ fun () ->
  let buf = Cstruct.of_string "x" in
  let op = Raw.allocate p 1 wr buf in
  ignore (Raw.submit_buffer op Raw.Write_file buf 0L);
  check "first delivery" 1 (Raw.dequeue p 10000);
  let first = Raw.completion op in
  assert (
    match first with
    | Some c -> c.id = 1 && c.bytes_transferred = 1 && c.error = None
    | None -> false);
  Raw.wake_port p;
  Gc.compact ();
  check "retry the same packet" 1 (Raw.dequeue p 0);
  assert (Raw.completion op = first);
  Raw.acknowledge p;
  check "next packet" 0 (Raw.dequeue p 0);
  Raw.acknowledge p;
  check "queue drained" (-1) (Raw.dequeue p 0)

let () =
  Alcotest.run "iocp"
    [
      ( "ports",
        List.map
          (fun (n, f) -> Alcotest.test_case n `Quick f)
          [
            ("closed port", closed_port);
            ("cross-domain wake", wake);
            ("consumer handover", consumer_handover);
            ("association ownership", association_ownership);
            ("failed registration", failed_registration);
            ("foreign alias after handle reuse", foreign_alias_reuse);
            ("retry delivery", retry_delivery);
            ("pipe modes", pipe_modes);
            ("file delete sharing", delete_sharing);
            ("file creation permissions", creation_permissions);
          ] );
    ]
