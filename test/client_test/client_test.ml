open! Base
open! Stdio

let ( let* ) = Lwt.bind

(* -- Daemon shared across all tests *)
let daemon = ref None

let get_daemon () =
  match !daemon with
  | Some d -> d
  | None -> failwith "daemon not started"

(* -- Helper: connect with optional tenant *)
let connect_client ?tenant ~name () =
  let d = get_daemon () in
  Test_harness.connect d ?tenant ~name ()

(* -- Tests: Registration *)

let test_registration _switch () =
  let d = get_daemon () in
  let* (conn, _events, transport) = Test_harness.connect d ~name:"reg-test" () in
  Alcotest.(check string) "registered as anonymous" "anonymous" (Client.tenant_name conn);
  Tcp_transport.close transport

let test_registration_with_tenant _switch () =
  let* (conn, _events, transport) = connect_client ~tenant:"my-tenant" ~name:"named" () in
  Alcotest.(check string) "registered as my-tenant" "my-tenant" (Client.tenant_name conn);
  Tcp_transport.close transport

(* -- Tests: Commands *)

let test_status _switch () =
  let* (conn, _events, transport) = connect_client ~name:"status-test" () in
  let* result = Client.call conn Protocol.Status () in
  (match result with
   | Ok status ->
     Alcotest.(check bool) "uptime >= 0" true (status.uptime_seconds >= 0)
   | Error e -> Alcotest.fail (Printf.sprintf "status failed: %s" e));
  Tcp_transport.close transport

let test_get_config _switch () =
  let* (conn, _events, transport) = connect_client ~name:"config-test" () in
  let* result = Client.call conn Protocol.Get_config () in
  (match result with
   | Ok config ->
     let has_test_tenant =
       List.exists config.Protocol.tenants ~f:(fun (id, _) -> String.equal id "test-tenant")
     in
     Alcotest.(check bool) "has test-tenant" true has_test_tenant
   | Error e -> Alcotest.fail (Printf.sprintf "get_config failed: %s" e));
  Tcp_transport.close transport

let test_get_rules _switch () =
  let* (conn, _events, transport) = connect_client ~name:"rules-test" () in
  let* result = Client.call conn Protocol.Get_rules () in
  (match result with
   | Ok rules ->
     Alcotest.(check bool) "has rules" true (List.length rules >= 1)
   | Error e -> Alcotest.fail (Printf.sprintf "get_rules failed: %s" e));
  Tcp_transport.close transport

let test_set_rules _switch () =
  let* (conn, _events, transport) = connect_client ~name:"set-rules-test" () in
  let new_rules : Protocol.rule list = [
    { pattern = "https://new[.]example[.]com/.*"; target = "test-tenant"; enabled = true };
  ] in
  let* result = Client.call conn Protocol.Set_rules new_rules in
  (match result with
   | Ok () -> ()
   | Error e -> Alcotest.fail (Printf.sprintf "set_rules failed: %s" e));
  (* Verify round-trip *)
  let* result = Client.call conn Protocol.Get_rules () in
  (match result with
   | Ok rules ->
     Alcotest.(check int) "one rule" 1 (List.length rules);
     Alcotest.(check string) "pattern" "https://new[.]example[.]com/.*"
       (List.hd_exn rules).pattern
   | Error e -> Alcotest.fail (Printf.sprintf "get_rules verify failed: %s" e));
  (* Restore original rules *)
  let original_rules : Protocol.rule list = [
    { pattern = "https?://www[.]example[.]com/.*"; target = "test-tenant"; enabled = true };
    { pattern = "https?://disabled[.]example[.]com/.*"; target = "test-tenant"; enabled = false };
  ] in
  let* _result = Client.call conn Protocol.Set_rules original_rules in
  Tcp_transport.close transport

(* -- Tests: Routing *)

let test_redirect _switch () =
  (* Target client registers as "test-tenant" *)
  let* (_target_conn, target_events, target_transport) =
    connect_client ~tenant:"test-tenant" ~name:"target" () in
  (* Source client registers as "alice" *)
  let* (src_conn, _src_events, src_transport) =
    connect_client ~tenant:"alice" ~name:"source" () in
  (* Source opens a URL matching the rule → should route to test-tenant *)
  let* result = Client.call src_conn Protocol.Open { url = "http://www.example.com/page" } in
  (match result with
   | Ok (Protocol.Remote tenant) ->
     Alcotest.(check string) "routed to test-tenant" "test-tenant" tenant
   | Ok Protocol.Local -> Alcotest.fail "expected Remote, got Local"
   | Error e -> Alcotest.fail (Printf.sprintf "open failed: %s" e));
  (* Target should receive Navigate push (skip Config_updated pushes) *)
  let rec await_navigate () =
    let* ev = Lwt_stream.next target_events in
    match ev with
    | Client.Push (Protocol.Navigate { url }) ->
      Alcotest.(check string) "navigate url" "http://www.example.com/page" url;
      Lwt.return_unit
    | Client.Push (Protocol.Config_updated _) -> await_navigate ()
    | _ -> Alcotest.fail "expected Navigate push on target"
  in
  let* () = await_navigate () in
  let* () = Tcp_transport.close target_transport in
  Tcp_transport.close src_transport

let test_no_redirect _switch () =
  let* (conn, _events, transport) = connect_client ~tenant:"alice" ~name:"no-redir" () in
  (* URL doesn't match any rule → Local *)
  let* result = Client.call conn Protocol.Open { url = "http://www.other.com/page" } in
  (match result with
   | Ok Protocol.Local -> ()
   | Ok (Protocol.Remote _) -> Alcotest.fail "expected Local, got Remote"
   | Error e -> Alcotest.fail (Printf.sprintf "open failed: %s" e));
  Tcp_transport.close transport

let test_self_open _switch () =
  (* Client registered as "test-tenant" opens URL targeting its own tenant → forced local *)
  let* (conn, _events, transport) =
    connect_client ~tenant:"test-tenant" ~name:"self" () in
  let* result = Client.call conn Protocol.Open { url = "http://www.example.com/self" } in
  (match result with
   | Ok Protocol.Local -> ()
   | Ok (Protocol.Remote _) -> Alcotest.fail "expected Local for self-open, got Remote"
   | Error e -> Alcotest.fail (Printf.sprintf "self-open failed: %s" e));
  Tcp_transport.close transport

let test_cooldown _switch () =
  (* Target must be registered to receive redirects *)
  let* (_target_conn, _target_events, target_transport) =
    connect_client ~tenant:"test-tenant" ~name:"cooldown-target" () in
  (* Source client *)
  let* (src_conn, _src_events, src_transport) =
    connect_client ~tenant:"alice" ~name:"cooldown-src" () in
  let url = "http://www.example.com/cooldown-test" in
  (* First open → Remote (starts cooldown) *)
  let* result = Client.call src_conn Protocol.Open { url } in
  (match result with
   | Ok (Protocol.Remote _) -> ()
   | Ok Protocol.Local -> Alcotest.fail "first open: expected Remote, got Local"
   | Error e -> Alcotest.fail (Printf.sprintf "first open failed: %s" e));
  (* Second open immediately → Local (cooldown active) *)
  let* result = Client.call src_conn Protocol.Open { url } in
  (match result with
   | Ok Protocol.Local -> ()
   | Ok (Protocol.Remote _) -> Alcotest.fail "second open: expected Local (cooldown), got Remote"
   | Error e -> Alcotest.fail (Printf.sprintf "second open failed: %s" e));
  (* Wait for cooldown to expire (1 second configured) *)
  let* () = Lwt_unix.sleep 1.1 in
  (* Third open → Remote again *)
  let* result = Client.call src_conn Protocol.Open { url } in
  (match result with
   | Ok (Protocol.Remote _) -> ()
   | Ok Protocol.Local -> Alcotest.fail "third open: expected Remote after cooldown, got Local"
   | Error e -> Alcotest.fail (Printf.sprintf "third open failed: %s" e));
  let* () = Tcp_transport.close target_transport in
  Tcp_transport.close src_transport

(* -- Test runner *)

let () =
  let d = Test_harness.start () in
  daemon := Some d;
  Lwt_main.run
    (Alcotest_lwt.run "client-integration" [
       ("registration", [
          Alcotest_lwt.test_case "anonymous" `Quick test_registration;
          Alcotest_lwt.test_case "named tenant" `Quick test_registration_with_tenant;
        ]);
       ("commands", [
          Alcotest_lwt.test_case "status" `Quick test_status;
          Alcotest_lwt.test_case "get_config" `Quick test_get_config;
          Alcotest_lwt.test_case "get_rules" `Quick test_get_rules;
          Alcotest_lwt.test_case "set_rules" `Quick test_set_rules;
        ]);
       ("routing", [
          Alcotest_lwt.test_case "redirect" `Quick test_redirect;
          Alcotest_lwt.test_case "no redirect" `Quick test_no_redirect;
          Alcotest_lwt.test_case "self-open forced local" `Quick test_self_open;
          Alcotest_lwt.test_case "cooldown" `Slow test_cooldown;
        ]);
     ]);
  Test_harness.stop d
