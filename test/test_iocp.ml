(* Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>
   SPDX-License-Identifier: MIT *)

let check = Alcotest.(check int)
let close_fd = Unix.close

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

let ports () =
  let p = Raw.create_port () in
  Fun.protect ~finally:(fun () -> Raw.close_port p) @@ fun () ->
  let r, w = Iocp.pipe () in
  Fun.protect ~finally:(fun () -> Unix.close r; Unix.close w) @@ fun () ->
  assert (Raw.same_handle r r);
  Raw.associate_port p r;
  Raw.detach r;
  Raw.wake_port p;
  check "notification" 0 (Raw.dequeue p 1000);
  Raw.acknowledge p;
  check "empty queue" (-1) (Raw.dequeue p 0)

let () = Alcotest.run "iocp" [ "files", [ Alcotest.test_case "pipe modes" `Quick pipe_modes; Alcotest.test_case "file delete sharing" `Quick delete_sharing; Alcotest.test_case "creation permissions" `Quick creation_permissions ]; "ports", [ Alcotest.test_case "port lifecycle" `Quick ports ] ]
