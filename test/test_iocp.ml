(* Self-contained test suite for ocaml-iocp.

   Exercises the high-level [Iocp] module for:
     - file I/O (positioned read/write, EOF);
     - stream sockets (in-process loopback AcceptEx/ConnectEx + send/recv + shutdown)
     - UDP datagrams (WSARecvFrom/WSASendTo with a peer address);
     - submission backpressure (pool exhaustion);
     - cancellation and the in-flight op count;
     - posted wakeup packets and the register-wait lifecycle. *)

let zero = Optint.Int63.zero

(* Submissions return [None] when the OVERLAPPED pool is exhausted. In tests
   that don't deliberately exhaust it, that is a failure. *)
let must label = function
  | Some x -> x
  | None -> Alcotest.failf "%s: submission returned None (overlapped pool exhausted)" label

(* Wait for one real I/O completion (fail on timeout or an unexpected posted
   packet). *)
let wait_io iocp : Iocp.completion_status =
  match Iocp.wait iocp ~timeout:5000 with
  | Some (Iocp.Io cs) -> cs
  | Some (Iocp.Posted _) -> Alcotest.fail "expected an I/O completion, got a posted packet"
  | None -> Alcotest.fail "timed out waiting for an IOCP completion"

(* Collect [n] I/O completions; their arrival order is not specified. *)
let wait_n_io iocp n = List.init n (fun _ -> wait_io iocp)

let by_id (css : Iocp.completion_status list) id =
  List.find_opt (fun (cs : Iocp.completion_status) -> Iocp.Id.equal cs.id id) css

let check_no_error label (cs : Iocp.completion_status) =
  match cs.error with
  | None -> ()
  | Some e -> Alcotest.failf "%s: unexpected error %s" label (Unix.error_message e)

let close_noerr fd = try Unix.close fd with Unix.Unix_error _ -> ()

(* {2 File operations} *)

let file_write_read () =
  let iocp = Iocp.create 8 in
  let fn = Filename.temp_file "iocp_rw" ".bin" in
  Fun.protect ~finally:(fun () -> Sys.remove fn) @@ fun () ->
  let h = Iocp.openfile iocp 1 fn Unix.[ O_CREAT; O_RDWR; O_TRUNC ] 0o600 in
  let data = "Test data for IOCP" in
  let src = Cstruct.of_string data in
  let wid = must "write" (Iocp.write iocp h (Cstruct.to_bigarray src) ~pos:0 ~len:(String.length data) ~off:zero) in
  let wcs = wait_io iocp in
  check_no_error "write" wcs;
  Alcotest.(check bool) "write id matches" true (Iocp.Id.equal wcs.id wid);
  Alcotest.(check int) "wrote all bytes" (String.length data) wcs.bytes_transferred;
  let dst = Cstruct.create (String.length data) in
  let rid = must "read" (Iocp.read iocp h (Cstruct.to_bigarray dst) ~pos:0 ~len:(String.length data) ~off:zero) in
  let rcs = wait_io iocp in
  check_no_error "read" rcs;
  Alcotest.(check bool) "read id matches" true (Iocp.Id.equal rcs.id rid);
  Alcotest.(check int) "read all bytes" (String.length data) rcs.bytes_transferred;
  Alcotest.(check string) "round-trip data" data (Cstruct.to_string dst);
  close_noerr (Iocp.Handle.fd h)

(* The file offset comes from the OVERLAPPED: a read at offset 6 of
   "HELLO-WORLD" yields "WORLD". *)
let file_positioned () =
  let iocp = Iocp.create 8 in
  let fn = Filename.temp_file "iocp_pos" ".bin" in
  Fun.protect ~finally:(fun () -> Sys.remove fn) @@ fun () ->
  let h = Iocp.openfile iocp 1 fn Unix.[ O_CREAT; O_RDWR; O_TRUNC ] 0o600 in
  let src = Cstruct.of_string "HELLO-WORLD" in
  let _ = must "write" (Iocp.write iocp h (Cstruct.to_bigarray src) ~pos:0 ~len:(Cstruct.length src) ~off:zero) in
  check_no_error "write" (wait_io iocp);
  let dst = Cstruct.create 5 in
  let _ = must "read" (Iocp.read iocp h (Cstruct.to_bigarray dst) ~pos:0 ~len:5 ~off:(Optint.Int63.of_int 6)) in
  let cs = wait_io iocp in
  check_no_error "read" cs;
  Alcotest.(check int) "read 5 bytes" 5 cs.bytes_transferred;
  Alcotest.(check string) "positioned read" "WORLD" (Cstruct.to_string dst);
  close_noerr (Iocp.Handle.fd h)

let file_read_past_eof () =
  let iocp = Iocp.create 8 in
  let fn = Filename.temp_file "iocp_eof" ".bin" in
  Fun.protect ~finally:(fun () -> Sys.remove fn) @@ fun () ->
  let oc = open_out_bin fn in
  output_string oc "abcd";
  close_out oc;
  let h = Iocp.openfile iocp 1 fn Unix.[ O_RDONLY ] 0 in
  let dst = Cstruct.create 4 in
  let _ = must "read" (Iocp.read iocp h (Cstruct.to_bigarray dst) ~pos:0 ~len:4 ~off:(Optint.Int63.of_int 4096)) in
  let cs = wait_io iocp in
  Alcotest.(check bool) "read past EOF reports an error" true (cs.error <> None);
  close_noerr (Iocp.Handle.fd h)

(* {2 Stream sockets} *)

(* Establish a connected TCP pair over loopback using one completion port: post
   AcceptEx and ConnectEx, drain both completions, then apply the
   accept/connect context updates. Returns (server, client, fds-to-close). *)
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
  let addr_buf = Iocp.Sockaddr.accept_buffer () in
  let accept_id = must "accept" (Iocp.accept iocp lfd server addr_buf) in
  (* ConnectEx requires the socket to be bound first. *)
  let client_u = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind client_u (Unix.ADDR_INET (Unix.inet_addr_any, 0));
  let client = Iocp.handle_of_fd iocp client_u 3 in
  let connect_id =
    must "connect" (Iocp.connect iocp client (Iocp.Sockaddr.of_unix (Unix.ADDR_INET (Unix.inet_addr_loopback, port))))
  in
  let css = wait_n_io iocp 2 in
  List.iter (check_no_error "handshake") css;
  Alcotest.(check bool) "accept completed" true (by_id css accept_id <> None);
  Alcotest.(check bool) "connect completed" true (by_id css connect_id <> None);
  Iocp.update_accept_ctx iocp ~listen:lfd server;
  Iocp.update_connect_ctx iocp client;
  (server, client, [ lfd_u; server_u; client_u ])

let socket_accept_connect () =
  let iocp = Iocp.create 8 in
  let _server, _client, fds = connected_pair iocp in
  List.iter close_noerr fds

let socket_send_recv () =
  let iocp = Iocp.create 8 in
  let server, client, fds = connected_pair iocp in
  Fun.protect ~finally:(fun () -> List.iter close_noerr fds) @@ fun () ->
  let msg = "ping-from-client" in
  let send_id = must "send" (Iocp.send iocp client [ Cstruct.of_string msg ]) in
  let rbuf = Cstruct.create 64 in
  let recv_id = must "recv" (Iocp.recv iocp server [ rbuf ]) in
  let css = wait_n_io iocp 2 in
  List.iter (check_no_error "send/recv") css;
  Alcotest.(check bool) "send completed" true (by_id css send_id <> None);
  let rcs =
    match by_id css recv_id with
    | Some c -> c
    | None -> Alcotest.fail "no recv completion"
  in
  Alcotest.(check int) "received byte count" (String.length msg) rcs.bytes_transferred;
  Alcotest.(check string) "received payload" msg
    (Cstruct.to_string (Cstruct.sub rbuf 0 rcs.bytes_transferred))

(* A graceful shutdown of the peer's send side surfaces as a 0-byte (EOF) read. *)
let socket_shutdown () =
  let iocp = Iocp.create 8 in
  let server, client, fds = connected_pair iocp in
  Fun.protect ~finally:(fun () -> List.iter close_noerr fds) @@ fun () ->
  Iocp.shutdown iocp client Unix.SHUTDOWN_SEND;
  let recv_id = must "recv" (Iocp.recv iocp server [ Cstruct.create 8 ]) in
  let cs = wait_io iocp in
  check_no_error "recv" cs;
  Alcotest.(check bool) "recv id matches" true (Iocp.Id.equal cs.id recv_id);
  Alcotest.(check int) "recv after peer shutdown is EOF (0 bytes)" 0 cs.bytes_transferred

(* {2 UDP datagrams} *)

let udp_socket iocp key =
  let u = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  Unix.bind u (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  let port = match Unix.getsockname u with Unix.ADDR_INET (_, p) -> p | _ -> assert false in
  (Iocp.handle_of_fd iocp u key, u, port)

let udp_send_recv_from () =
  let iocp = Iocp.create 8 in
  let recv_h, recv_u, _recv_port = udp_socket iocp 1 in
  let send_h, send_u, send_port = udp_socket iocp 2 in
  Fun.protect ~finally:(fun () -> close_noerr recv_u; close_noerr send_u) @@ fun () ->
  let recv_port = match Unix.getsockname recv_u with Unix.ADDR_INET (_, p) -> p | _ -> assert false in
  let msg = "datagram-payload" in
  let src = Iocp.Sockaddr.create () in
  let rbuf = Cstruct.create 64 in
  let recv_id = must "recv_from" (Iocp.recv_from iocp recv_h [ rbuf ] src) in
  let dst = Iocp.Sockaddr.of_unix (Unix.ADDR_INET (Unix.inet_addr_loopback, recv_port)) in
  let send_id = must "send_to" (Iocp.send_to iocp send_h [ Cstruct.of_string msg ] dst) in
  let css = wait_n_io iocp 2 in
  List.iter (check_no_error "udp") css;
  Alcotest.(check bool) "send_to completed" true (by_id css send_id <> None);
  let rcs = match by_id css recv_id with Some c -> c | None -> Alcotest.fail "no recv_from completion" in
  Alcotest.(check int) "datagram byte count" (String.length msg) rcs.bytes_transferred;
  Alcotest.(check string) "datagram payload" msg
    (Cstruct.to_string (Cstruct.sub rbuf 0 rcs.bytes_transferred));
  (* The source address is the sender's bound port. *)
  (match Iocp.Sockaddr.get src with
   | Unix.ADDR_INET (_, p) -> Alcotest.(check int) "source port is sender's" send_port p
   | _ -> Alcotest.fail "expected an INET source address")

(* {2 Backpressure, cancellation, active-op count} *)

(* With a pool of 4, four pending recvs are accepted and the fifth returns None;
   cancelling them then drains the in-flight count back to zero. *)
let pool_exhaustion_and_cancel () =
  let iocp = Iocp.create ~overlapped:4 1 in
  let server, _client, fds = connected_pair iocp in
  Fun.protect ~finally:(fun () -> List.iter close_noerr fds) @@ fun () ->
  let ids = List.filter_map Fun.id (List.init 4 (fun _ -> Iocp.recv iocp server [ Cstruct.create 8 ])) in
  Alcotest.(check int) "four recvs accepted" 4 (List.length ids);
  Alcotest.(check int) "four ops in flight" 4 (Iocp.active_ops iocp);
  Alcotest.(check bool) "fifth recv is None (pool exhausted)" true
    (Iocp.recv iocp server [ Cstruct.create 8 ] = None);
  List.iter (Iocp.cancel iocp) ids;
  let n = ref 0 in
  while Iocp.active_ops iocp > 0 && !n < 10 do
    ignore (Iocp.wait iocp ~timeout:1000);
    incr n
  done;
  Alcotest.(check int) "all ops drained after cancel" 0 (Iocp.active_ops iocp)

let cancel_reports_error () =
  let iocp = Iocp.create 8 in
  let server, _client, fds = connected_pair iocp in
  Fun.protect ~finally:(fun () -> List.iter close_noerr fds) @@ fun () ->
  let id = must "recv" (Iocp.recv iocp server [ Cstruct.create 8 ]) in
  Alcotest.(check int) "one op in flight" 1 (Iocp.active_ops iocp);
  Iocp.cancel iocp id;
  let cs = wait_io iocp in
  Alcotest.(check bool) "completion is for the cancelled op" true (Iocp.Id.equal cs.id id);
  Alcotest.(check bool) "cancelled recv reports an error" true (cs.error <> None);
  Alcotest.(check int) "no ops in flight afterwards" 0 (Iocp.active_ops iocp)

(* {2 Wakeups / posted packets} *)

let post_packet () =
  let iocp = Iocp.create 1 in
  Iocp.post iocp ~key:42 ~bytes:7;
  match Iocp.wait iocp ~timeout:1000 with
  | Some (Iocp.Posted { key; bytes_transferred }) ->
    Alcotest.(check int) "posted key" 42 key;
    Alcotest.(check int) "posted bytes" 7 bytes_transferred
  | Some (Iocp.Io _) -> Alcotest.fail "expected a posted packet, got an I/O completion"
  | None -> Alcotest.fail "no packet arrived"

let wakeup_packet () =
  let iocp = Iocp.create 1 in
  Iocp.wakeup iocp;
  match Iocp.wait iocp ~timeout:1000 with
  | Some (Iocp.Posted { key; _ }) -> Alcotest.(check int) "wakeup uses key 0" 0 key
  | _ -> Alcotest.fail "expected a wakeup packet"

(* Exercise the register/unregister C lifecycle. The signal -> posted-packet
   delivery itself is the same code path as [post] (verified above); here we
   register a wait on a handle that will not fire and tear it down cleanly. *)
let register_wait_lifecycle () =
  let iocp = Iocp.create 1 in
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () -> close_noerr s) @@ fun () ->
  let w = Iocp.register_wait iocp s ~key:99 in
  (* Drain a posted packet if the wait happens to fire; not required to. *)
  ignore (Iocp.wait iocp ~timeout:50);
  Iocp.unregister_wait w;
  Iocp.unregister_wait w (* idempotent: must not double free *)


(* {2 Argument checking}

   The kernel touches these buffers after the call returns, so a length that
   runs past the end of one corrupts the heap instead of failing. They have to
   be rejected at submission. *)

let rejects_oversized_read () =
  let iocp = Iocp.create ~overlapped:4 1 in
  Fun.protect ~finally:(fun () -> Iocp.close iocp) @@ fun () ->
  let fn = Filename.temp_file "iocp_bounds" ".bin" in
  Fun.protect ~finally:(fun () -> try Sys.remove fn with Sys_error _ -> ()) @@ fun () ->
  let h = Iocp.openfile iocp 1 fn Unix.[ O_CREAT; O_RDWR; O_TRUNC ] 0o600 in
  let small = Cstruct.to_bigarray (Cstruct.create 16) in
  let bad op = Alcotest.check_raises op (Invalid_argument op) in
  bad "Iocp.read: buffer too small"
    (fun () -> ignore (Iocp.read iocp h small ~pos:0 ~len:4096 ~off:zero));
  bad "Iocp.read: buffer too small"
    (fun () -> ignore (Iocp.read iocp h small ~pos:8 ~len:16 ~off:zero));
  bad "Iocp.write: buffer too small"
    (fun () -> ignore (Iocp.write iocp h small ~pos:0 ~len:4096 ~off:zero));
  (* A rejected submission must not keep the slot it drew from the pool. *)
  Alcotest.(check int) "nothing in flight" 0 (Iocp.active_ops iocp);
  let id = must "write" (Iocp.write iocp h small ~pos:0 ~len:16 ~off:zero) in
  let cs = wait_io iocp in
  check_no_error "write" cs;
  Alcotest.(check bool) "pool still usable" true (Iocp.Id.equal cs.id id)

let rejects_undersized_accept_buffer () =
  let iocp = Iocp.create ~overlapped:4 1 in
  Fun.protect ~finally:(fun () -> Iocp.close iocp) @@ fun () ->
  let lfd_u = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let sock_u = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () -> close_noerr lfd_u; close_noerr sock_u) @@ fun () ->
  Unix.bind lfd_u (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen lfd_u 1;
  let lfd = Iocp.handle_of_fd iocp lfd_u 1 in
  let sock = Iocp.handle_of_fd iocp sock_u 2 in
  Alcotest.check_raises "too small"
    (Invalid_argument "Iocp.accept: buffer smaller than Sockaddr.accept_buffer_size")
    (fun () -> ignore (Iocp.accept iocp lfd sock (Cstruct.create 4)));
  Alcotest.(check int) "nothing in flight" 0 (Iocp.active_ops iocp)

let () =
  Alcotest.run "iocp"
    [ ( "file",
        [ Alcotest.test_case "write_read" `Quick file_write_read;
          Alcotest.test_case "positioned" `Quick file_positioned;
          Alcotest.test_case "read_past_eof" `Quick file_read_past_eof ] );
      ( "socket",
        [ Alcotest.test_case "accept_connect" `Quick socket_accept_connect;
          Alcotest.test_case "send_recv" `Quick socket_send_recv;
          Alcotest.test_case "shutdown" `Quick socket_shutdown ] );
      ( "udp",
        [ Alcotest.test_case "send_recv_from" `Quick udp_send_recv_from ] );
      ( "backpressure",
        [ Alcotest.test_case "pool_exhaustion_and_cancel" `Quick pool_exhaustion_and_cancel;
          Alcotest.test_case "cancel_reports_error" `Quick cancel_reports_error ] );
      ( "arguments",
        [ Alcotest.test_case "oversized read/write" `Quick rejects_oversized_read;
          Alcotest.test_case "undersized accept buffer" `Quick rejects_undersized_accept_buffer ] );
      ( "wakeup",
        [ Alcotest.test_case "post" `Quick post_packet;
          Alcotest.test_case "wakeup" `Quick wakeup_packet;
          Alcotest.test_case "register_wait_lifecycle" `Quick register_wait_lifecycle ] ) ]
