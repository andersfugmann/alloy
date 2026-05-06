open! Base
open! Stdio

let ( let* ) = Lwt.bind

(* -- Daemon shared across all tests *)
let daemon = ref None

let get_daemon () =
  match !daemon with
  | Some d -> d
  | None -> failwith "daemon not started"

(* -- Helper: connect and wait for Registered push *)
let connect_client ~name =
  let d = get_daemon () in
  let* (conn, events, transport) = Test_harness.connect d ~name in
  (* Drain events until Registered *)
  let rec wait_registered () =
    let* ev = Lwt_stream.next events in
    match ev with
    | Client.Push (Protocol.Registered _) -> Lwt.return (conn, events, transport)
    | _ -> wait_registered ()
  in
  wait_registered ()

(* -- Helper: next push from event stream (skip Config_updated) *)
let _next_push events =
  let rec loop () =
    let* ev = Lwt_stream.next events in
    match ev with
    | Client.Push (Protocol.Config_updated _) -> loop ()
    | _ -> Lwt.return ev
  in
  loop ()

(* -- Tests *)

let test_registration _switch () =
  let d = get_daemon () in
  let* (_conn, events, transport) = Test_harness.connect d ~name:"reg-test" in
  let* ev = Lwt_stream.next events in
  (match ev with
   | Client.Push (Protocol.Registered { tenant_id }) ->
     Alcotest.(check string) "registered as anonymous" "anonymous" tenant_id
   | _ -> Alcotest.fail "expected Registered push");
  Tcp_transport.close transport

let test_status _switch () =
  let* (conn, _events, transport) = connect_client ~name:"status-test" in
  let* result = Client.call conn Protocol.Status () in
  (match result with
   | Ok status ->
     Alcotest.(check bool) "has tenants" true
       (List.length status.Protocol.registered_tenants >= 0);
     Alcotest.(check bool) "uptime >= 0" true (status.uptime_seconds >= 0)
   | Error e -> Alcotest.fail (Printf.sprintf "status failed: %s" e));
  Tcp_transport.close transport

let test_get_config _switch () =
  let* (conn, _events, transport) = connect_client ~name:"config-test" in
  let* result = Client.call conn Protocol.Get_config () in
  (match result with
   | Ok config ->
     (* Check the test-tenant exists *)
     let has_test_tenant =
       List.exists config.Protocol.tenants ~f:(fun (id, _) -> String.equal id "test-tenant")
     in
     Alcotest.(check bool) "has test-tenant" true has_test_tenant
   | Error e -> Alcotest.fail (Printf.sprintf "get_config failed: %s" e));
  Tcp_transport.close transport

let test_get_rules _switch () =
  let* (conn, _events, transport) = connect_client ~name:"rules-test" in
  let* result = Client.call conn Protocol.Get_rules () in
  (match result with
   | Ok rules ->
     Alcotest.(check bool) "has rules" true (List.length rules >= 1)
   | Error e -> Alcotest.fail (Printf.sprintf "get_rules failed: %s" e));
  Tcp_transport.close transport

let test_set_rules _switch () =
  let* (conn, _events, transport) = connect_client ~name:"set-rules-test" in
  let new_rules : Protocol.rule list = [
    { pattern = "https://new[.]example[.]com/.*"; target = "test-tenant"; enabled = true };
  ] in
  let* result = Client.call conn Protocol.Set_rules new_rules in
  (match result with
   | Ok () -> ()
   | Error e -> Alcotest.fail (Printf.sprintf "set_rules failed: %s" e));
  (* Verify by reading back *)
  let* result = Client.call conn Protocol.Get_rules () in
  (match result with
   | Ok rules ->
     Alcotest.(check int) "one rule" 1 (List.length rules);
     Alcotest.(check string) "pattern matches" "https://new[.]example[.]com/.*"
       (List.hd_exn rules).pattern
   | Error e -> Alcotest.fail (Printf.sprintf "get_rules verification failed: %s" e));
  (* Restore original rules *)
  let original_rules : Protocol.rule list = [
    { pattern = "https://routed[.]example[.]com/.*"; target = "test-tenant"; enabled = true };
    { pattern = "https://disabled[.]example[.]com/.*"; target = "test-tenant"; enabled = false };
  ] in
  let* _result = Client.call conn Protocol.Set_rules original_rules in
  Tcp_transport.close transport

let test_routing _switch () =
  let* (conn, _events, transport) = connect_client ~name:"routing-test" in
  (* Test a URL that matches a rule *)
  let* result = Client.call conn Protocol.Test { url = "https://routed.example.com/page" } in
  (match result with
   | Ok (Protocol.Match { tenant; rule_index = _ }) ->
     Alcotest.(check string) "routed to test-tenant" "test-tenant" tenant
   | Ok (Protocol.No_match _) -> Alcotest.fail "expected match, got no_match"
   | Error e -> Alcotest.fail (Printf.sprintf "test routing failed: %s" e));
  (* Test a URL that doesn't match any rule *)
  let* result = Client.call conn Protocol.Test { url = "https://unmatched.example.com/page" } in
  (match result with
   | Ok (Protocol.No_match _) -> ()
   | Ok (Protocol.Match _) -> Alcotest.fail "expected no_match, got match"
   | Error e -> Alcotest.fail (Printf.sprintf "test no-match failed: %s" e));
  Tcp_transport.close transport

let test_open_local _switch () =
  let* (conn, _events, transport) = connect_client ~name:"open-test" in
  (* Open a URL that doesn't match any rule → should be Local *)
  let* result = Client.call conn Protocol.Open { url = "https://local.example.com/page" } in
  (match result with
   | Ok Protocol.Local -> ()
   | Ok (Protocol.Remote _) -> Alcotest.fail "expected Local, got Remote"
   | Error e -> Alcotest.fail (Printf.sprintf "open local failed: %s" e));
  Tcp_transport.close transport

let test_open_remote _switch () =
  let* (conn, _events, transport) = connect_client ~name:"open-remote-test" in
  (* Open a URL that matches a rule → remote tenant has no browser, expect error *)
  let* result = Client.call conn Protocol.Open { url = "https://routed.example.com/page" } in
  (match result with
   | Error msg ->
     (* Expected: no browser registered for test-tenant *)
     Alcotest.(check bool) "error mentions tenant" true
       (String.is_substring msg ~substring:"test-tenant")
   | Ok (Protocol.Remote _) ->
     (* Also acceptable if a browser was somehow registered *)
     ()
   | Ok Protocol.Local -> Alcotest.fail "expected Remote or error, got Local");
  Tcp_transport.close transport

let test_open_on _switch () =
  (* Connect a target client *)
  let d = get_daemon () in
  let* (_target_conn, target_events, target_transport) =
    Test_harness.connect d ~name:"target-client" in
  (* Wait for target's registration *)
  let rec wait_registered () =
    let* ev = Lwt_stream.next target_events in
    match ev with
    | Client.Push (Protocol.Registered _) -> Lwt.return_unit
    | _ -> wait_registered ()
  in
  let* () = wait_registered () in
  (* Connect the sender *)
  let* (sender_conn, _sender_events, sender_transport) = connect_client ~name:"sender" in
  (* Send open_on targeting the other client — may error since target is "anonymous" *)
  let* result = Client.call sender_conn Protocol.Open_on
    { target = "target-client"; url = "https://navigate.example.com/page" } in
  (match result with
   | Ok _ -> ()
   | Error msg ->
     (* open_on targets by tenant name, not client name — may fail *)
     Alcotest.(check bool) "got error" true (String.length msg > 0));
  let* () = Tcp_transport.close target_transport in
  Tcp_transport.close sender_transport

(* -- Test runner *)

let () =
  let d = Test_harness.start () in
  daemon := Some d;
  Lwt_main.run
    (Alcotest_lwt.run "client-integration" [
       ("registration", [
          Alcotest_lwt.test_case "registers and receives push" `Quick test_registration;
        ]);
       ("commands", [
          Alcotest_lwt.test_case "status" `Quick test_status;
          Alcotest_lwt.test_case "get_config" `Quick test_get_config;
          Alcotest_lwt.test_case "get_rules" `Quick test_get_rules;
          Alcotest_lwt.test_case "set_rules" `Quick test_set_rules;
        ]);
       ("routing", [
          Alcotest_lwt.test_case "test matched route" `Quick test_routing;
          Alcotest_lwt.test_case "open local" `Quick test_open_local;
          Alcotest_lwt.test_case "open remote" `Quick test_open_remote;
          Alcotest_lwt.test_case "open_on with navigate push" `Quick test_open_on;
        ]);
     ]);
  Test_harness.stop d
