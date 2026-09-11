(* Stress tests for ocaml-iocp.

   These are about volume and lifecycle rather than protocol correctness, which
   test_iocp.ml already covers. A completion port owns two resources that are
   easy to lose track of one at a time and obvious in bulk: the pool of
   OVERLAPPED structures that operations are submitted on, and the port handle
   itself. Each test below runs enough work that a per-operation or per-port
   leak becomes a hard failure rather than a slow drift.

   They are sized to finish in a few seconds so they can run on every build. *)

external handle_count : unit -> int = "ocaml_iocp_test_handle_count"

let zero = Optint.Int63.zero

let must label = function
  | Some x -> x
  | None -> Alcotest.failf "%s: submission returned None (overlapped pool exhausted)" label

let wait_io iocp : Iocp.completion_status =
  match Iocp.wait iocp ~timeout:10_000 with
  | Some (Iocp.Io cs) -> cs
  | Some (Iocp.Posted _) -> Alcotest.fail "expected an I/O completion, got a posted packet"
  | None -> Alcotest.fail "timed out waiting for an IOCP completion"

let check_no_error label (cs : Iocp.completion_status) =
  match cs.error with
  | None -> ()
  | Some e -> Alcotest.failf "%s: unexpected error %s" label (Unix.error_message e)

let close_noerr fd = try Unix.close fd with Unix.Unix_error _ -> ()

let with_temp_file name f =
  let fn = Filename.temp_file name ".bin" in
  Fun.protect ~finally:(fun () -> try Sys.remove fn with Sys_error _ -> ()) (fun () -> f fn)

(* As in test_iocp.ml: a connected TCP pair over loopback driven by one port. *)
let connected_pair iocp =
  let lfd_u = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt lfd_u Unix.SO_REUSEADDR true;
  Unix.bind lfd_u (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen lfd_u 1;
  let port =
    match Unix.getsockname lfd_u with
    | Unix.ADDR_INET (_, p) -> p
    | _ -> assert false
  in
  let lfd = Iocp.handle_of_fd iocp lfd_u 1 in
  let server_u = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let server = Iocp.handle_of_fd iocp server_u 2 in
  let accept_id = must "accept" (Iocp.accept iocp lfd server (Iocp.Sockaddr.accept_buffer ())) in
  let client_u = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind client_u (Unix.ADDR_INET (Unix.inet_addr_any, 0));
  let client = Iocp.handle_of_fd iocp client_u 3 in
  let connect_id =
    must "connect"
      (Iocp.connect iocp client
         (Iocp.Sockaddr.of_unix (Unix.ADDR_INET (Unix.inet_addr_loopback, port))))
  in
  List.iter (fun _ -> check_no_error "handshake" (wait_io iocp)) [ accept_id; connect_id ];
  Iocp.update_accept_ctx iocp ~listen:lfd server;
  Iocp.update_connect_ctx iocp client;
  (server, client, [ lfd_u; server_u; client_u ])

(* {2 The OVERLAPPED pool} *)

(* Every completed operation must give its slot back, or a long-lived port stops
   accepting work. A pool of 8 running 20_000 writes only survives if it does. *)
let slot_churn () =
  let iocp = Iocp.create ~overlapped:8 1 in
  Fun.protect ~finally:(fun () -> Iocp.close iocp) @@ fun () ->
  with_temp_file "iocp_churn" @@ fun fn ->
  let h = Iocp.openfile iocp 1 fn Unix.[ O_CREAT; O_RDWR; O_TRUNC ] 0o600 in
  let buf = Cstruct.to_bigarray (Cstruct.of_string "0123456789abcdef") in
  for i = 1 to 20_000 do
    let off = Optint.Int63.of_int ((i mod 64) * 16) in
    ignore (must "write" (Iocp.write iocp h buf ~pos:0 ~len:16 ~off) : Iocp.Id.t);
    check_no_error "write" (wait_io iocp)
  done;
  Alcotest.(check int) "no operations left in flight" 0 (Iocp.active_ops iocp)

(* A submission that fails outright never goes pending, so no completion will
   arrive to release its slot. Before this was handled, four failures were
   enough to wedge a pool of four for good. *)
let failed_submissions () =
  let iocp = Iocp.create ~overlapped:4 1 in
  Fun.protect ~finally:(fun () -> Iocp.close iocp) @@ fun () ->
  with_temp_file "iocp_failed" @@ fun fn ->
  (* Write-only, so reading it fails synchronously with ERROR_ACCESS_DENIED. *)
  let h = Iocp.openfile iocp 1 fn Unix.[ O_CREAT; O_WRONLY; O_TRUNC ] 0o600 in
  let buf = Cstruct.to_bigarray (Cstruct.create 16) in
  for _ = 1 to 5_000 do
    match Iocp.read iocp h buf ~pos:0 ~len:16 ~off:zero with
    | Some _ -> Alcotest.fail "read of a write-only handle should have failed"
    | None -> Alcotest.fail "pool exhausted: a failed submission kept its slot"
    | exception Unix.Unix_error _ -> ()
  done;
  Alcotest.(check int) "no operations left in flight" 0 (Iocp.active_ops iocp);
  (* The pool must still be fully usable. *)
  let ok = Iocp.write iocp h buf ~pos:0 ~len:16 ~off:zero in
  check_no_error "write after failures" (wait_io iocp);
  Alcotest.(check bool) "pool still accepts work" true (ok <> None)

(* Fill the pool to its limit, confirm the next submission is refused rather
   than overrunning it, then drain and do it again. *)
let saturate_and_drain () =
  let depth = 16 in
  let iocp = Iocp.create ~overlapped:depth 1 in
  Fun.protect ~finally:(fun () -> Iocp.close iocp) @@ fun () ->
  with_temp_file "iocp_saturate" @@ fun fn ->
  let h = Iocp.openfile iocp 1 fn Unix.[ O_CREAT; O_RDWR; O_TRUNC ] 0o600 in
  let buf = Cstruct.to_bigarray (Cstruct.of_string "0123456789abcdef") in
  for _ = 1 to 100 do
    for i = 0 to depth - 1 do
      let off = Optint.Int63.of_int (i * 16) in
      ignore (must "write" (Iocp.write iocp h buf ~pos:0 ~len:16 ~off) : Iocp.Id.t)
    done;
    Alcotest.(check int) "pool full" depth (Iocp.active_ops iocp);
    Alcotest.(check bool) "further submissions are refused" true
      (Iocp.write iocp h buf ~pos:0 ~len:16 ~off:zero = None);
    for _ = 1 to depth do check_no_error "write" (wait_io iocp) done;
    Alcotest.(check int) "drained" 0 (Iocp.active_ops iocp)
  done

(* A cancelled operation still reports a completion, so the accounting has to
   come out even however many are cancelled. *)
let cancel_churn () =
  let iocp = Iocp.create ~overlapped:8 1 in
  let server, _client, fds = connected_pair iocp in
  Fun.protect ~finally:(fun () -> List.iter close_noerr fds; Iocp.close iocp) @@ fun () ->
  for _ = 1 to 2_000 do
    let id = must "recv" (Iocp.recv iocp server [ Cstruct.create 64 ]) in
    Iocp.cancel iocp id;
    let cs = wait_io iocp in
    Alcotest.(check bool) "completion is for the cancelled op" true (Iocp.Id.equal cs.id id)
  done;
  Alcotest.(check int) "no operations left in flight" 0 (Iocp.active_ops iocp)

(* {2 Bulk data} *)

(* Push several megabytes through a socket pair keeping one send and one recv in
   flight, and check the bytes arrive intact. Exercises WSABUF lifetime: the
   buffers are only kept alive by the port's record of the in-flight op. *)
let bulk_transfer () =
  let iocp = Iocp.create ~overlapped:8 1 in
  let server, client, fds = connected_pair iocp in
  Fun.protect ~finally:(fun () -> List.iter close_noerr fds; Iocp.close iocp) @@ fun () ->
  let chunk = 64 * 1024 in
  let chunks = 64 in                                   (* 4 MiB *)
  let src = Cstruct.create chunk in
  for i = 0 to chunk - 1 do Cstruct.set_uint8 src i (i land 0xff) done;
  let chunk_sum =
    let s = ref 0 in
    for i = 0 to chunk - 1 do s := !s + Cstruct.get_uint8 src i done;
    !s
  in
  let total = chunks * chunk in
  let sent = ref 0 and received = ref 0 and sum = ref 0 in
  let sends_left = ref chunks in
  let send_id = ref None and recv_id = ref None in
  let recv_buf = Cstruct.create chunk in
  let post_send () =
    if !sends_left > 0 then begin
      decr sends_left;
      send_id := Some (must "send" (Iocp.send iocp client [ src ]))
    end else send_id := None
  in
  let post_recv () = recv_id := Some (must "recv" (Iocp.recv iocp server [ recv_buf ])) in
  let is id = match id with Some i -> Iocp.Id.equal i | None -> fun _ -> false in
  post_send ();
  post_recv ();
  (* Keep exactly one send and one recv outstanding, replacing each as it
     completes, so neither direction stalls on a full socket buffer. *)
  while !send_id <> None || !recv_id <> None do
    let cs = wait_io iocp in
    check_no_error "bulk" cs;
    if is !send_id cs.id then begin
      sent := !sent + cs.bytes_transferred;
      post_send ()
    end else if is !recv_id cs.id then begin
      if cs.bytes_transferred = 0 then Alcotest.fail "peer closed mid-transfer";
      for i = 0 to cs.bytes_transferred - 1 do
        sum := !sum + Cstruct.get_uint8 recv_buf i
      done;
      received := !received + cs.bytes_transferred;
      if !received < total then post_recv () else recv_id := None
    end else Alcotest.fail "completion for an operation we did not submit"
  done;
  Alcotest.(check int) "sent everything" total !sent;
  Alcotest.(check int) "received everything" total !received;
  Alcotest.(check int) "bytes arrived intact" (chunks * chunk_sum) !sum

(* Receive exactly [n] bytes, scattered across several buffers per call, and
   return them reassembled. A recv may return short, so keep going. *)
let recv_exact iocp sock n =
  let acc = Buffer.create n in
  while Buffer.length acc < n do
    let want = n - Buffer.length acc in
    let bufs = List.init 4 (fun _ -> Cstruct.create ((want + 3) / 4)) in
    ignore (must "recv" (Iocp.recv iocp sock bufs) : Iocp.Id.t);
    let cs = wait_io iocp in
    check_no_error "recv" cs;
    if cs.bytes_transferred = 0 then Alcotest.fail "unexpected end of stream";
    (* Fill order follows the buffer list. *)
    let left = ref cs.bytes_transferred in
    List.iter
      (fun b ->
        let take = min !left (Cstruct.length b) in
        Buffer.add_string acc (Cstruct.to_string (Cstruct.sub b 0 take));
        left := !left - take)
      bufs
  done;
  Buffer.contents acc

(* Several buffers in one call, to exercise building the WSABUF array and
   keeping it alive for the duration of the operation. *)
let scatter_gather () =
  let iocp = Iocp.create ~overlapped:8 1 in
  let server, client, fds = connected_pair iocp in
  Fun.protect ~finally:(fun () -> List.iter close_noerr fds; Iocp.close iocp) @@ fun () ->
  let parts = List.init 16 (fun i -> Printf.sprintf "%04d" i) in
  let expect = String.concat "" parts in
  for _ = 1 to 200 do
    ignore (must "send" (Iocp.send iocp client (List.map Cstruct.of_string parts)) : Iocp.Id.t);
    check_no_error "send" (wait_io iocp);
    Alcotest.(check string) "round-trip" expect
      (recv_exact iocp server (String.length expect))
  done;
  Alcotest.(check int) "no operations left in flight" 0 (Iocp.active_ops iocp)

(* {2 The port handle} *)

(* Ports are kernel handles. Creating and closing many of them must not leave
   any behind; a small allowance absorbs handles the runtime happens to open
   while the test runs. *)
let many_ports () =
  let n = 500 in
  Gc.full_major ();
  let before = handle_count () in
  for _ = 1 to n do
    let t = Iocp.create ~overlapped:1 1 in
    Iocp.close t
  done;
  Gc.full_major ();
  let growth = handle_count () - before in
  if growth > 16 then
    Alcotest.failf "%d handles still held after creating and closing %d ports" growth n

(* Closing twice, and closing a port that still has work in flight, are both
   allowed and must not raise. *)
let close_is_idempotent () =
  let iocp = Iocp.create ~overlapped:4 1 in
  with_temp_file "iocp_close" @@ fun fn ->
  let h = Iocp.openfile iocp 1 fn Unix.[ O_CREAT; O_RDWR; O_TRUNC ] 0o600 in
  let buf = Cstruct.to_bigarray (Cstruct.of_string "0123456789abcdef") in
  ignore (must "write" (Iocp.write iocp h buf ~pos:0 ~len:16 ~off:zero) : Iocp.Id.t);
  Iocp.close iocp;
  Iocp.close iocp

let () =
  Alcotest.run "IOCP stress"
    [
      ( "overlapped pool",
        [
          Alcotest.test_case "slot churn" `Slow slot_churn;
          Alcotest.test_case "failed submissions" `Slow failed_submissions;
          Alcotest.test_case "saturate and drain" `Slow saturate_and_drain;
          Alcotest.test_case "cancel churn" `Slow cancel_churn;
        ] );
      ( "bulk data",
        [
          Alcotest.test_case "bulk transfer" `Slow bulk_transfer;
          Alcotest.test_case "scatter/gather" `Slow scatter_gather;
        ] );
      ( "port handles",
        [
          Alcotest.test_case "many ports" `Slow many_ports;
          Alcotest.test_case "close is idempotent" `Quick close_is_idempotent;
        ] );
    ]
