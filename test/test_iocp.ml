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

let delivery p =
  match Iocp.wait p ~timeout:10000 with
  | Some (Iocp.Completion (data, c)) -> data, c
  | Some Wake -> Alcotest.fail "unexpected wake"
  | None -> Alcotest.fail "completion timed out"

let status p = snd (delivery p)

let success p id bytes =
  let data, c = delivery p in
  check "id" id data;
  check "bytes" bytes c.bytes_transferred;
  match c.error with
  | None -> ()
  | Some e -> Alcotest.fail (Unix.error_message e)

let with_port ?(skip_on_success = false) fn =
  let p = Iocp.create ~skip_on_success () in
  Fun.protect
    ~finally:(fun () ->
      Iocp.cancel_all p;
      while Iocp.pending p > 0 do
        ignore (status p)
      done;
      close_port p)
    (fun () -> fn p)

let with_pipe ?(skip_on_success = false) fn =
  let r, w = Iocp.pipe () in
  Fun.protect
    ~finally:(fun () ->
      close_fd r;
      close_fd w)
    (fun () -> with_port ~skip_on_success (fun p -> fn p r w))

let invalid fn =
  match fn () with
  | () -> Alcotest.fail "expected Invalid_argument"
  | exception Invalid_argument _ -> ()

let file () =
  let path = Filename.temp_file "iocp-λ-" ".tmp" in
  Fun.protect ~finally:(fun () -> Sys.remove path) @@ fun () ->
  let fd = Iocp.openfile path [ O_RDWR; O_TRUNC; O_CLOEXEC ] 0o600 in
  Fun.protect ~finally:(fun () -> close_fd fd) @@ fun () ->
  with_port @@ fun p ->
  ignore
    (Iocp.write p ~data:1 (handle p fd) (Cstruct.of_string "hello") ~offset:8L);
  success p 1 5;
  let buf = Cstruct.create 5 in
  ignore (Iocp.read p ~data:2 (handle p fd) buf ~offset:8L);
  success p 2 5;
  Alcotest.(check string) "contents" "hello" (Cstruct.to_string buf);
  (* A truncated high offset would read the five bytes at offset 8. *)
  for id = 3 to 102 do
    ignore (Iocp.read p ~data:id (handle p fd) buf ~offset:0x1_0000_0008L);
    success p id 0;
    assert (Iocp.wait p ~timeout:0 = None)
  done;
  check "no pending" 0 (Iocp.pending p)

let pipe () =
  with_pipe @@ fun p r w ->
  let buf = Cstruct.create 5 in
  ignore (Iocp.read p ~data:1 (handle p r) buf ~offset:0L);
  ignore
    (Iocp.write p ~data:2 (handle p w) (Cstruct.of_string "hello") ~offset:0L);
  let ia, a = delivery p and ib, b = delivery p in
  check "id sum" 3 (ia + ib);
  check "read/write bytes" 10 (a.bytes_transferred + b.bytes_transferred);
  assert (a.error = None && b.error = None);
  Alcotest.(check string) "contents" "hello" (Cstruct.to_string buf)

let cancel_gc () =
  with_pipe @@ fun p r _w ->
  let request =
    Iocp.read p ~data:1 (handle p r) (Cstruct.create 100) ~offset:0L
  in
  Gc.full_major ();
  Gc.compact ();
  Iocp.cancel p request;
  let data, c = delivery p in
  check "cancel id" 1 data;
  assert (c.error <> None);
  Iocp.cancel p request;
  check "drained" 0 (Iocp.pending p)

let completed_buffer () =
  List.iter
    (fun skip_on_success ->
      with_pipe ~skip_on_success @@ fun p _ w ->
      let h = handle p w in
      let weak = Weak.create 1 in
      let request =
        let buf = Cstruct.create 1 in
        Weak.set weak 0 (Some buf);
        Iocp.write p ~data:1 h buf ~offset:0L
      in
      let c = match Iocp.result request with Some c -> c | None -> status p in
      assert (c.error = None);
      Gc.full_major ();
      Alcotest.(check bool)
        "completed buffer released" false (Weak.check weak 0);
      ignore (Sys.opaque_identity request))
    [ false; true ]

let lifetime () =
  with_pipe @@ fun p r _w ->
  ignore (Iocp.read p ~data:1 (handle p r) (Cstruct.create 1) ~offset:0L);
  invalid (fun () -> Iocp.close p);
  ignore (Iocp.read p ~data:1 (handle p r) (Cstruct.create 1) ~offset:0L);
  check "same payload, distinct requests" 2 (Iocp.pending p);
  invalid (fun () ->
      ignore (Iocp.read p ~data:2 (handle p r) (Cstruct.create 1) ~offset:(-2L)))

let submission_failure () =
  with_pipe @@ fun p _r w ->
  for _ = 1 to 1100 do
    match Iocp.read p ~data:1 (handle p w) (Cstruct.create 1) ~offset:0L with
    | _ -> Alcotest.fail "read on write-only pipe succeeded"
    | exception Unix.Unix_error _ -> check "rolled back" 0 (Iocp.pending p)
  done

let many_requests () =
  with_pipe @@ fun p r _w ->
  for id = 1 to 1100 do
    ignore (Iocp.read p ~data:id (handle p r) (Cstruct.create 1) ~offset:0L)
  done;
  Gc.full_major ();
  check "pending" 1100 (Iocp.pending p);
  Iocp.cancel_all p;
  for _ = 1 to 1100 do
    assert ((status p).error <> None)
  done;
  check "drained" 0 (Iocp.pending p)

let wake () =
  with_port @@ fun p ->
  assert (Iocp.wait p ~timeout:0 = None);
  let d = Domain.spawn (fun () -> Iocp.wake p) in
  assert (Iocp.wait p ~timeout:10000 = Some Wake);
  Domain.join d

let sockets () =
  let listener = Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0 in
  let client = Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0 in
  let accepted = Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () ->
      List.iter close_fd [ listener; client; accepted ])
  @@ fun () ->
  Unix.bind listener (ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen listener 1;
  Unix.bind client (ADDR_INET (Unix.inet_addr_any, 0));
  with_port @@ fun p ->
  ignore (Iocp.accept p ~data:1 (handle p listener) ~client:accepted);
  ignore (Iocp.connect p ~data:2 (handle p client) (Unix.getsockname listener));
  let ia, a = delivery p and ib, b = delivery p in
  check "ids" 3 (ia + ib);
  assert (a.error = None && b.error = None);
  assert (Unix.getpeername client = Unix.getsockname listener);
  ignore (Unix.getpeername accepted);
  let buf = Cstruct.create 5 in
  ignore (Iocp.recv p ~data:3 (handle p accepted) buf);
  ignore (Iocp.send p ~data:4 (handle p client) (Cstruct.of_string "hello"));
  let a = status p and b = status p in
  assert (a.error = None && b.error = None);
  check "bytes" 10 (a.bytes_transferred + b.bytes_transferred);
  Alcotest.(check string) "socket contents" "hello" (Cstruct.to_string buf)

let refused () =
  let reserved = Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0 in
  let client = Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () ->
      close_fd client;
      close_fd reserved)
  @@ fun () ->
  Unix.bind reserved (ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.bind client (ADDR_INET (Unix.inet_addr_any, 0));
  with_port @@ fun p ->
  match Iocp.connect p ~data:1 (handle p client) (Unix.getsockname reserved) with
  | _ ->
      let c = status p in
      if c.error <> Some Unix.ECONNREFUSED then
        Alcotest.fail
          (match c.error with
          | None -> "unexpected success"
          | Some e -> Printexc.to_string (Unix.Unix_error (e, "connect", "")))
  | exception Unix.Unix_error (ECONNREFUSED, _, _) -> ()

let receive_shutdown ~duplicate () =
  let a, b = Unix.socketpair ~cloexec:true PF_UNIX SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () ->
      close_fd a;
      close_fd b)
  @@ fun () ->
  let other_a, other_b = Unix.socketpair ~cloexec:true PF_UNIX SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () ->
      close_fd other_a;
      close_fd other_b)
  @@ fun () ->
  with_port @@ fun p ->
  ignore (Iocp.recv p ~data:1 (handle p a) (Cstruct.create 1));
  let other = Iocp.recv p ~data:2 (handle p other_a) (Cstruct.create 1) in
  if duplicate then
    let alias = Unix.dup ~cloexec:true a in
    Fun.protect
      ~finally:(fun () -> close_fd alias)
      (fun () -> Iocp.shutdown p (handle p alias) Unix.SHUTDOWN_RECEIVE)
  else Iocp.shutdown p (handle p a) Unix.SHUTDOWN_RECEIVE;
  let data, c = delivery p in
  (match c.error with
  | None -> ()
  | Some e ->
      Alcotest.fail
        (Printexc.to_string (Unix.Unix_error (e, "shutdown read", ""))));
  check "shutdown request" 1 data;
  check "EOF" 0 c.bytes_transferred;
  assert (Iocp.wait p ~timeout:0 = None);
  check "unrelated receive remains pending" 1 (Iocp.pending p);
  Iocp.cancel p other;
  let data, c = delivery p in
  check "unrelated request" 2 data;
  assert (c.error <> None)

let shutdown_preserves_error () =
  List.iter
    (fun skip_on_success ->
      let p = Iocp.create ~skip_on_success () in
      let r = Unix.socket ~cloexec:true PF_INET SOCK_DGRAM 0 in
      let w = Unix.socket ~cloexec:true PF_INET SOCK_DGRAM 0 in
      Fun.protect ~finally:(fun () ->
          close_fd r;
          close_fd w;
          close_port p)
      @@ fun () ->
      Unix.bind r (ADDR_INET (Unix.inet_addr_loopback, 0));
      Unix.bind w (ADDR_INET (Unix.inet_addr_loopback, 0));
      ignore (Iocp.recvfrom p ~data:1 (handle p r) (Cstruct.create 1));
      let addr = Unix.getsockname r in
      ignore (Unix.sendto w (Bytes.make 1024 'x') 0 1024 [] addr);
      ignore (Unix.sendto w (Bytes.of_string "done") 0 4 [] addr);
      let marker = Bytes.create 4 in
      check "marker length" 4 (fst (Unix.recvfrom r marker 0 4 []));
      Alcotest.(check string) "marker" "done" (Bytes.to_string marker);
      Iocp.shutdown p (handle p r) Unix.SHUTDOWN_RECEIVE;
      let c = status p in
      assert (c.error = Some Unix.EMSGSIZE);
      check "partial receive" 1 c.bytes_transferred;
      assert (c.source = Some (Unix.getsockname w)))
    [ false; true ]

(* Model a failure while converting a dequeued packet, before delivery commits. *)
let retry_delivery () =
  let slot = Raw.slot () in
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
  let op = Raw.allocate p 1 wr buf Raw.Native_queue in
  ignore (Raw.submit_buffer op Raw.Write_file buf 0L);
  check "first delivery" 1 (Raw.dequeue p slot 10000);
  let first = Raw.completion op false in
  assert (first.bytes_transferred = 1 && first.error = None);
  Raw.wake_port p;
  Gc.compact ();
  check "retry the same packet" 1 (Raw.dequeue p slot 0);
  assert (Raw.completion op false = first);
  Raw.acknowledge slot;
  check "next packet" 0 (Raw.dequeue p slot 0);
  Raw.acknowledge slot;
  check "queue drained" (-1) (Raw.dequeue p slot 0)

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

let concurrent_delivery () =
  with_pipe @@ fun p r w ->
  let readers = Array.init 2 (fun _ -> Domain.spawn (fun () -> fst (delivery p))) in
  ignore (Iocp.read p ~data:1 (handle p r) (Cstruct.create 1) ~offset:0L);
  ignore (Iocp.write p ~data:2 (handle p w) (Cstruct.of_string "x") ~offset:0L);
  assert (Array.to_list (Array.map Domain.join readers) |> List.sort compare = [1; 2])

let concurrent_wait () =
  with_port @@ fun p ->
  let targets = Array.init 2 (fun _ -> Atomic.make None) in
  let workers =
    Array.map
      (fun target ->
        Domain.spawn (fun () ->
            let interrupt = Iocp.Interrupt.current () in
            Fun.protect ~finally:(fun () -> Iocp.Interrupt.close interrupt)
            @@ fun () ->
            Atomic.set target (Some interrupt);
            Iocp.wait p ~timeout:10000))
      targets
  in
  let rec get target =
    match Atomic.get target with
    | Some t -> t
    | None ->
        Domain.cpu_relax ();
        get target
  in
  let first = get targets.(0) in
  ignore (get targets.(1));
  Iocp.Interrupt.signal first;
  assert (Domain.join workers.(0) = None);
  Iocp.wake p;
  assert (Domain.join workers.(1) = Some Iocp.Wake);
  Iocp.Interrupt.signal first

let cancel_race () =
  with_pipe @@ fun p r w ->
  for n = 1 to 100 do
    let read =
      Iocp.read p ~data:(2 * n) (handle p r) (Cstruct.create 1) ~offset:0L
    in
    ignore
      (Iocp.write p
         ~data:((2 * n) + 1)
         (handle p w) (Cstruct.of_string "x") ~offset:0L);
    Iocp.cancel p read;
    let ia, _ = delivery p and ib, _ = delivery p in
    check "two distinct completions" ((4 * n) + 1) (ia + ib);
    assert (ia <> ib);
    check "drained" 0 (Iocp.pending p)
  done

let cross_domain_submit () =
  with_pipe @@ fun p r w ->
  let buf = Cstruct.create 5 in
  ignore (Iocp.read p ~data:1 (handle p r) buf ~offset:0L);
  let writer =
    Domain.spawn (fun () ->
        ignore
          (Iocp.write p ~data:2 (handle p w)
             (Cstruct.of_string "hello")
             ~offset:0L))
  in
  Domain.join writer;
  Gc.full_major ();
  let ia, a = delivery p and ib, b = delivery p in
  assert (a.error = None && b.error = None);
  check "ids" 3 (ia + ib);
  Alcotest.(check string) "contents" "hello" (Cstruct.to_string buf)

let closed_port () =
  let p = Iocp.create () in
  close_port p;
  close_port p;
  invalid (fun () -> Iocp.wake p);
  invalid (fun () -> ignore (Iocp.wait p ~timeout:0))

let wrong_port () =
  with_pipe @@ fun p r _ ->
  with_port @@ fun other ->
  let request = Iocp.read p ~data:1 (handle p r) (Cstruct.create 1) ~offset:0L in
  invalid (fun () -> Iocp.cancel other request)

let cancel_accept () =
  let listener = Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0 in
  let client = Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () ->
      close_fd listener;
      close_fd client)
  @@ fun () ->
  Unix.bind listener (ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen listener 1;
  with_port @@ fun p ->
  let r = Iocp.accept p ~data:1 (handle p listener) ~client in
  Gc.full_major ();
  Iocp.cancel p r;
  let data, c = delivery p in
  check "cancelled accept ID" 1 data;
  assert (c.error <> None)

let append_access () =
  let path = Filename.temp_file "iocp-append" ".tmp" in
  Fun.protect ~finally:(fun () -> Sys.remove path) @@ fun () ->
  let fd = Iocp.openfile path [ O_RDONLY; O_APPEND ] 0 in
  Fun.protect ~finally:(fun () -> close_fd fd) @@ fun () ->
  with_port @@ fun p ->
  match
    Iocp.write p ~data:1 (handle p fd) (Cstruct.of_string "x") ~offset:(-1L)
  with
  | _ -> Alcotest.fail "append granted write access to a read-only handle"
  | exception Unix.Unix_error _ -> check "rollback" 0 (Iocp.pending p)

let datagrams () =
  let a = Unix.socket ~cloexec:true PF_INET SOCK_DGRAM 0 in
  let b = Unix.socket ~cloexec:true PF_INET SOCK_DGRAM 0 in
  Fun.protect ~finally:(fun () ->
      close_fd a;
      close_fd b)
  @@ fun () ->
  Unix.bind a (ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.bind b (ADDR_INET (Unix.inet_addr_loopback, 0));
  with_port @@ fun p ->
  let cancelled = Iocp.recvfrom p ~data:1 (handle p a) (Cstruct.create 5) in
  Gc.full_major ();
  Iocp.cancel p cancelled;
  assert ((status p).error <> None);
  List.iter
    (fun msg ->
      let buf = Cstruct.create 5 in
      ignore (Iocp.recvfrom p ~data:2 (handle p a) buf);
      ignore
        (Iocp.sendto p ~data:3 (handle p b) (Cstruct.of_string msg)
           (Unix.getsockname a));
      let id, x = delivery p and y = status p in
      assert (x.error = None && y.error = None);
      let received = if id = 2 then x else y in
      check "datagram length" (String.length msg) received.bytes_transferred;
      assert (received.source = Some (Unix.getsockname b));
      Alcotest.(check string)
        "datagram contents" msg
        (Cstruct.to_string (Cstruct.sub buf 0 received.bytes_transferred)))
    [ "hello"; "" ]

let buffered_truncation () =
  List.iter
    (fun skip_on_success ->
      let udp () = Unix.socket ~cloexec:true PF_INET SOCK_DGRAM 0 in
      let r = udp () and w = udp () in
      Fun.protect ~finally:(fun () -> close_fd r; close_fd w) @@ fun () ->
      List.iter (fun s -> Unix.bind s (ADDR_INET (Unix.inet_addr_loopback, 0))) [ r; w ];
      with_port ~skip_on_success @@ fun p ->
      ignore (Unix.sendto w (Bytes.of_string "abc") 0 3 [] (Unix.getsockname r));
      let buf = Cstruct.create 1 in
      let req = Iocp.recvfrom p ~data:1 (handle p r) buf in
      let c = match Iocp.result req with Some c -> c | None -> status p in
      assert (c.error = Some Unix.EMSGSIZE);
      assert (c.source = Some (Unix.getsockname w));
      check "copied bytes" 1 c.bytes_transferred;
      Alcotest.(check string) "prefix" "a" (Cstruct.to_string buf))
    [ false; true ]

let cancel_write () =
  with_pipe @@ fun p _r w ->
  let r =
    Iocp.write p ~data:1 (handle p w) (Cstruct.create (1024 * 1024)) ~offset:0L
  in
  assert (Iocp.wait p ~timeout:0 = None);
  Gc.full_major ();
  Iocp.cancel p r;
  let data, c = delivery p in
  check "write cancellation ID" 1 data;
  assert (c.error <> None);
  check "write drained" 0 (Iocp.pending p)

let pipe_eof () =
  let r, w = Iocp.pipe () in
  Fun.protect ~finally:(fun () -> close_fd r) @@ fun () ->
  with_port @@ fun p ->
  ignore (Iocp.read p ~data:1 (handle p r) (Cstruct.create 1) ~offset:0L);
  close_fd w;
  success p 1 0;
  for id = 2 to 20 do
    ignore (Iocp.read p ~data:id (handle p r) (Cstruct.create 1) ~offset:0L);
    success p id 0;
    assert (Iocp.wait p ~timeout:0 = None)
  done

let notification () =
  let module W = Iocp.Wakeup in
  let t = W.create () in
  Fun.protect ~finally:(fun () -> W.close t) @@ fun () ->
  W.wait t 0;
  invalid (fun () -> W.wait t (-2));
  W.signal t;
  W.signal t;
  W.wait t (-1);
  W.wait t (-1);
  let started = Atomic.make 0 in
  let waiter () =
    Atomic.incr started;
    W.wait t (-1)
  in
  let a = Domain.spawn waiter and b = Domain.spawn waiter in
  while Atomic.get started < 2 do
    Domain.cpu_relax ()
  done;
  W.close t;
  Domain.join a;
  Domain.join b;
  W.signal t;
  W.wait t (-1);
  invalid (fun () -> W.wait t (-2))

let immediate () =
  let p = Iocp.create ~skip_on_success:true () in
  let r, w = Iocp.pipe () in
  Fun.protect ~finally:(fun () ->
      close_port p;
      close_fd r;
      close_fd w)
  @@ fun () ->
  let inline = ref 0 in
  let finish request =
    let c =
      match Iocp.result request with
      | Some c ->
          incr inline;
          assert (Iocp.result request = Some c);
          c
      | None -> status p
    in
    assert (c.error = None);
    check "one byte" 1 c.bytes_transferred
  in
  for _ = 1 to 100 do
    finish (Iocp.write p ~data:1 (handle p w) (Cstruct.of_string "x") ~offset:0L);
    let buf = Cstruct.create 1 in
    finish (Iocp.read p ~data:2 (handle p r) buf ~offset:0L);
    Alcotest.(check string) "pipe contents" "x" (Cstruct.to_string buf);
    check "drained" 0 (Iocp.pending p);
    assert (Iocp.wait p ~timeout:0 = None)
  done;
  Alcotest.(check bool) "used inline completions" true (!inline > 0);
  let eof, writer = Iocp.pipe () in
  close_fd writer;
  Fun.protect ~finally:(fun () -> close_fd eof) @@ fun () ->
  for _ = 1 to 20 do
    let request =
      Iocp.read p ~data:3 (handle p eof) (Cstruct.create 1) ~offset:0L
    in
    let c = match Iocp.result request with Some c -> c | None -> status p in
    assert (c.error = None);
    check "EOF" 0 c.bytes_transferred;
    assert (Iocp.wait p ~timeout:0 = None)
  done

let open_flags () =
  let path = Filename.temp_file "iocp-open" ".tmp" in
  Sys.remove path;
  Fun.protect ~finally:(fun () -> Sys.remove path) @@ fun () ->
  let with_file flags fn =
    let fd = Iocp.openfile path flags 0o600 in
    Fun.protect ~finally:(fun () -> close_fd fd) (fun () -> fn fd)
  in
  with_port @@ fun p ->
  with_file [ O_WRONLY; O_CREAT; O_EXCL ] (fun fd ->
      ignore
        (Iocp.write p ~data:1 (handle p fd) (Cstruct.of_string "abc") ~offset:0L);
      success p 1 3);
  (match Iocp.openfile path [ O_WRONLY; O_CREAT; O_EXCL ] 0o600 with
  | fd ->
      close_fd fd;
      Alcotest.fail "exclusive creation replaced a file"
  | exception Unix.Unix_error (EEXIST, _, _) -> ());
  with_file [ O_WRONLY; O_APPEND ] (fun fd ->
      ignore
        (Iocp.write p ~data:2 (handle p fd) (Cstruct.of_string "d") ~offset:0L);
      success p 2 1);
  with_file [ O_RDONLY ] (fun fd ->
      let buf = Cstruct.create 4 in
      ignore (Iocp.read p ~data:3 (handle p fd) buf ~offset:0L);
      success p 3 4;
      Alcotest.(check string)
        "append cannot overwrite" "abcd" (Cstruct.to_string buf));
  with_file [ O_RDWR; O_TRUNC ] (fun fd ->
      check "truncated" 0 (Unix.fstat fd).st_size)

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
          (Unix.fstat reference_fd).st_perm (Unix.fstat fd).st_perm;
        with_port (fun p ->
            if mode = Unix.O_RDONLY then (
              ignore
                (Iocp.read p ~data:1 (handle p fd) (Cstruct.create 1) ~offset:0L);
              success p 1 0)
            else (
              check "Unix creation handle remains writable" 1
                (Unix.write_substring reference_fd "x" 0 1);
              ignore
                (Iocp.write p ~data:1 (handle p fd) (Cstruct.of_string "x")
                   ~offset:0L);
              success p 1 1))
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

let replaced_descriptor () =
  List.iter
    (fun skip_on_success ->
      let p = Iocp.create ~skip_on_success () in
      let target, writer = Iocp.pipe () in
      close_fd writer;
      Fun.protect ~finally:(fun () ->
          close_fd target;
          close_port p)
      @@ fun () ->
      associate p target;
      let finish data r =
        match Iocp.result r with Some c -> data, c | None -> delivery p
      in
      for n = 1 to 100 do
        let source, writer = Iocp.pipe () in
        Fun.protect ~finally:(fun () ->
            close_fd source;
            close_fd writer)
        @@ fun () ->
        release_matching (fun (_, original, _) -> original == target);
        Unix.dup2 source target;
        Unix.dup2 source target;
        if n mod 10 = 0 then Gc.compact ();
        let buf = Cstruct.create 1 in
        let read = Iocp.read p ~data:(2 * n) (handle p target) buf ~offset:0L in
        let write =
          Iocp.write p
            ~data:((2 * n) + 1)
            (handle p writer) (Cstruct.of_string "x") ~offset:0L
        in
        let ia, a = finish (2 * n) read and ib, b = finish (2 * n + 1) write in
        assert (a.error = None && b.error = None);
        check "both completions" ((4 * n) + 1) (ia + ib);
        Alcotest.(check string)
          "replacement contents" "x" (Cstruct.to_string buf);
        check "drained" 0 (Iocp.pending p)
      done)
    [ false; true ]

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

let typed_payload () =
  let p = Iocp.create () in
  Fun.protect ~finally:(fun () -> Iocp.close p) @@ fun () ->
  let r, w = Iocp.pipe () in
  Fun.protect ~finally:(fun () -> Unix.close r; Unix.close w) @@ fun () ->
  let h = Iocp.register p w in
  Fun.protect ~finally:(fun () -> Iocp.close_handle h) @@ fun () ->
  let token = ref "done" in
  for _ = 1 to 2 do
    ignore (Iocp.write p ~data:token h (Cstruct.of_string "x") ~offset:0L)
  done;
  Gc.compact ();
  for _ = 1 to 2 do
    let returned, c = delivery p in
    assert (returned == token && c.bytes_transferred = 1 && c.error = None)
  done

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

let registered_aliases ~skip_on_success () =
  with_pipe ~skip_on_success @@ fun p r w ->
  let alias = Unix.dup ~cloexec:true r in
  Fun.protect ~finally:(fun () -> Unix.close alias) @@ fun () ->
  assert (Iocp.same_handle r alias);
  assert (not (Iocp.same_handle r w));
  with_registration p r @@ fun original ->
  with_registration p alias @@ fun duplicate ->
  with_registration p w @@ fun writer ->
  assert (Iocp.file_key original == Iocp.file_key duplicate);
  assert (Iocp.file_key original != Iocp.file_key writer);
  let buf = Cstruct.create 1 in
  let read = Iocp.read p ~data:1 duplicate buf ~offset:0L in
  invalid (fun () -> Iocp.close_handle duplicate);
  Iocp.close_handle original;
  invalid (fun () -> ignore (Iocp.read p ~data:3 original buf ~offset:0L));
  let write = Iocp.write p ~data:2 writer (Cstruct.of_string "x") ~offset:0L in
  let finish data r = match Iocp.result r with Some c -> data, c | None -> delivery p in
  let ia, a = finish 1 read and ib, b = finish 2 write in
  assert (a.error = None && b.error = None);
  check "both completions" 3 (ia + ib);
  Alcotest.(check string) "alias contents" "x" (Cstruct.to_string buf)

let registered_pending () =
  with_pipe @@ fun p r w ->
  with_registration p r @@ fun reader ->
  with_registration p w @@ fun writer ->
  ignore (Iocp.read p ~data:1 reader (Cstruct.create 1) ~offset:0L);
  let pending = Iocp.read p ~data:2 reader (Cstruct.create 1) ~offset:0L in
  ignore (Iocp.write p ~data:3 writer (Cstruct.of_string "x") ~offset:0L);
  let ia, a = delivery p and ib, b = delivery p in
  assert (a.error = None && b.error = None);
  check "first read and write" 4 (ia + ib);
  invalid (fun () -> Iocp.close_handle reader);
  Iocp.cancel p pending;
  assert ((status p).error <> None);
  Iocp.close_handle reader

let registration_reuse () =
  let r, w = Iocp.pipe () in
  Fun.protect ~finally:(fun () ->
      Unix.close r;
      Unix.close w)
  @@ fun () ->
  List.iter
    (fun skip_on_success ->
      let p = Iocp.create ~skip_on_success () in
      Fun.protect ~finally:(fun () -> Iocp.close p) @@ fun () ->
      with_registration p r @@ fun reader ->
      with_registration p w @@ fun writer ->
      let finish request =
        let immediate = Iocp.result request in
        assert (immediate <> None = skip_on_success);
        let c = match immediate with Some c -> c | None -> status p in
        assert (c.error = None);
        check "one byte" 1 c.bytes_transferred
      in
      finish (Iocp.write p ~data:1 writer (Cstruct.of_string "x") ~offset:0L);
      let buf = Cstruct.create 1 in
      finish (Iocp.read p ~data:2 reader buf ~offset:0L);
      Alcotest.(check string)
        "re-registered contents" "x" (Cstruct.to_string buf);
      assert (Iocp.wait p ~timeout:0 = None))
    [ true; false; true; false ]

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

let () =
  Alcotest.run "iocp"
    [
      ( "completion",
        List.map
          (fun (n, f) -> Alcotest.test_case n `Quick f)
          [
            ("file creation and append flags", open_flags);
            ("file creation permissions", creation_permissions);
            ("file delete sharing", delete_sharing);
            ("replaced descriptor", replaced_descriptor);
            ("association ownership", association_ownership);
            ("failed registration", failed_registration);
            ("registered aliases", registered_aliases ~skip_on_success:false);
            ( "registered aliases with immediate completion",
              registered_aliases ~skip_on_success:true );
            ("registration with multiple pending requests", registered_pending);
            ("registration reuse across modes", registration_reuse);
            ("foreign alias after handle reuse", foreign_alias_reuse);
            ("immediate completion without duplicates", immediate);
            ("scheduler notification", notification);
            ("datagrams and cancellation", datagrams);
            ("cancel blocked pipe write", cancel_write);
            ("pipe EOF", pipe_eof);
            ("append preserves access", append_access);
            ("cancel accept", cancel_accept);
            ("cancellation race", cancel_race);
            ("cross-domain submission", cross_domain_submit);
            ("closed port", closed_port);
            ("typed completion payload", typed_payload);
            ("wrong-port cancellation", wrong_port);
            ("concurrent completion delivery", concurrent_delivery);
            ("targeted concurrent wait", concurrent_wait);
            ("sequential consumer handover", consumer_handover);
            ("retry unacknowledged delivery", retry_delivery);
            ("shutdown preserves queued errors", shutdown_preserves_error);
            ("buffered datagram truncation", buffered_truncation);
            ("receive shutdown", receive_shutdown ~duplicate:false);
            ("receive shutdown through dup", receive_shutdown ~duplicate:true);
            ("file and 64-bit offset", file);
            ("pipe", pipe);
            ("cancel and GC", cancel_gc);
            ("lifetime", lifetime);
            ("completed buffer lifetime", completed_buffer);
            ("submission rollback", submission_failure);
            ("more than 1024 requests", many_requests);
            ("cross-domain wake", wake);
            ("accept/connect/send/recv", sockets);
            ("connection refusal", refused);
          ] );
    ]
