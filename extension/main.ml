open! Base
open! Stdio
open Js_of_ocaml

let ( let* ) = Lwt.bind
let log = Chrome_api.log

(* -- Typed wrappers around Chrome APIs *)

let create_tab = Chrome_api.Tabs.create_url
let on_before_navigate = Chrome_api.Web_navigation.on_before_navigate
let create_context_menu = Chrome_api.Context_menus.create
let create_child_context_menu = Chrome_api.Context_menus.create_child
let remove_all_context_menus = Chrome_api.Context_menus.remove_all
let on_context_menu_clicked = Chrome_api.Context_menus.on_clicked
let on_installed = Chrome_api.Runtime.on_installed
let on_startup = Chrome_api.Runtime.on_startup

(* -- URL filtering *)

let is_internal_url = Page_util.is_internal_url

(* -- Event types for the coordinator *)

type native_port = Chrome_api.port

type event =
  | Navigation of { url : string; tab_id : int }
  | Bridge_event of Client.event
  | Port_disconnected
  | Connect_requested
  | Connect_with_settings of { port : native_port; tenant_name : string; daemon_host : string; daemon_port : string; debug_logging : bool }
  | Connection_ready of Client.connection
  | Context_menu of { menu_id : string; link_url : string; page_url : string; tab_id : int option }
  | Popup_query of { json : Yojson.Safe.t; respond : Yojson.Safe.t -> unit }
  | Setup_menus
  | Refresh_menus of { tenants : (string * string * bool) list }
  | Page_port_message of { port : Chrome_api.port; raw : string; frame : Protocol.frame }
  | Page_port_disconnected of { port : Chrome_api.port }
  | Subclient_response of string

type state = {
  connection : Client.connection option;
  tenant_names : (string * string * bool) list;
  self_tenant_id : string option;
  debug_logging : bool;
  subclient_ports : Chrome_api.port list;
  subclient_pending : (int * Chrome_api.port) Map.M(Int).t;
  subclient_next_id : int;
  subclient_queue : (int * string) list;
}

(* -- Event stream *)

let (event_stream, push_event) =
  Lwt_stream.create ()

let push ev = push_event (Some ev)

(* -- State operations *)

let initial_state = {
  connection = None;
  tenant_names = [];
  self_tenant_id = None;
  debug_logging = false;
  subclient_ports = [];
  subclient_pending = Map.empty (module Int);
  subclient_next_id = 1;
  subclient_queue = [];
}

let debug state msg =
  match state.debug_logging with
  | true -> log msg
  | false -> ()

let is_connected state =
  Option.is_some state.connection

let update_badge connected =
  match connected with
  | true ->
    Chrome_api.Action.set_icon
      "icons/icon16_connected.png"
      "icons/icon48_connected.png"
      "icons/icon128_connected.png"
  | false ->
    Chrome_api.Action.set_icon
      "icons/icon16_disconnected.png"
      "icons/icon48_disconnected.png"
      "icons/icon128_disconnected.png"

let call : type req resp. state -> (req, resp) Protocol.command -> req ->
    (resp, string) Result.t Lwt.t =
  fun state cmd request ->
    match state.connection with
    | None -> Lwt.return (Error "not connected")
    | Some conn ->
      log (Printf.sprintf "-> %s" (Protocol.command_name cmd));
      Client.call conn cmd request

(* -- JSON helpers *)

let string_field json key =
  match json with
  | `Assoc pairs ->
    (match List.Assoc.find pairs ~equal:String.equal key with
     | Some (`String s) -> Ok s
     | Some _ -> Error (Printf.sprintf "field %s: expected string" key)
     | None -> Error (Printf.sprintf "missing field: %s" key))
  | _ -> Error "expected JSON object"

(* -- Connection management *)

let non_empty s =
  match String.is_empty s with
  | true -> None
  | false -> Some s

let connect_with_settings port tenant_name daemon_host daemon_port ~debug_logging =
  let brand = non_empty (Chrome_api.Navigator.get_browser_brand ()) in
  let write msg = Chrome_api.Port.post_message_json port msg in
  let (read, push_incoming) = Lwt_stream.create () in
  Chrome_api.Port.on_message_json port (fun msg -> push_incoming (Some msg));
  Chrome_api.Port.on_disconnect port (fun () -> push Port_disconnected);
  (* Determine host/port for bridge handshake *)
  let host =
    match String.is_empty daemon_host with
    | true -> Constants.default_host
    | false -> daemon_host
  in
  let port_num =
    match Int.of_string_opt daemon_port with
    | Some p -> p
    | None -> Constants.default_port
  in
  (* Send bridge handshake, then init client *)
  Lwt.async (fun () ->
    let addr : Protocol.listen_address = { host; port = port_num } in
    let req = Protocol.make_bridge_request ~debug:debug_logging addr in
    write (Yojson.Safe.to_string (Protocol.bridge_request_to_yojson req));
    (* Wait for bridge response *)
    let* raw = Lwt_stream.next read in
    let hostname =
      match Yojson.Safe.from_string raw with
      | json ->
        begin match Protocol.parse_bridge_response json with
        | Ok connected -> connected.hostname
        | Error _ -> ""
        end
      | exception Yojson.Json_error _ -> ""
    in
    let name =
      match non_empty tenant_name with
      | Some n -> Some n
      | None ->
        match String.is_empty hostname with
        | true -> None
        | false -> Some hostname
    in
    log (Printf.sprintf "Bridge connected: hostname=%s, tenant=%s"
      hostname (Option.value name ~default:"(default)"));
    let* (conn, bridge_events) = Client.init ~write ~read ?name ?brand () in
    push (Connection_ready conn);
    let rec forward () =
      let* ev = Lwt_stream.next bridge_events in
      push (Bridge_event ev);
      forward ()
    in
    Lwt.catch forward (fun _exn -> Lwt.return_unit));
  { connection = None; tenant_names = []; self_tenant_id = None; debug_logging;
    subclient_ports = []; subclient_pending = Map.empty (module Int); subclient_next_id = 1;
    subclient_queue = [] }

let connect _state =
  match
    let p = Chrome_api.Runtime.connect_native "alloy" in
    log "Connected to native messaging host";
    p
  with
  | p ->
    Chrome_api.Storage.get_local [ "tenant_name"; "daemon_host"; "daemon_port"; "debug_logging" ]
      ~on_result:(fun pairs ->
        let find k =
          List.Assoc.find pairs ~equal:String.equal k
          |> Option.value ~default:""
        in
        push
          (Connect_with_settings
             {
               port = p;
               tenant_name = find "tenant_name";
               daemon_host = find "daemon_host";
               daemon_port = find "daemon_port";
               debug_logging = String.equal (find "debug_logging") "true";
             }));
    (* connection stays None until connect_with_settings — prevents race *)
    initial_state
  | exception exn ->
    log (Printf.sprintf "Failed to connect: %s" (Exn.to_string exn));
    initial_state

(* -- Event handlers (return state Lwt.t, use let* for commands) *)

let handle_push state (p : Protocol.push) =
  match p with
  | Navigate { url } ->
    log (Printf.sprintf "Received NAVIGATE push: %s" url);
    create_tab url;
    state
  | Registered _ ->
    (* Client.init consumes the Registered push; this is unreachable
       unless the server re-registers mid-session *)
    state
  | Config_updated { config = cfg; registered_tenants } ->
    log (Printf.sprintf "Config push: %d tenants, %d registered"
      (List.length cfg.tenants) (List.length registered_tenants));
    let registered_set = Set.of_list (module String) registered_tenants in
    let tenants = List.map cfg.tenants ~f:(fun (id, tc) ->
      (id, tc.Protocol.label, Set.mem registered_set id)) in
    push (Refresh_menus { tenants });
    state

let handle_navigation state url tab_id =
  match is_connected state && not (is_internal_url url) with
  | false -> Lwt.return state
  | true ->
    let t0 = Chrome_api.performance_now () in
    debug state (Printf.sprintf "→ Open %s" url);
    let* result = call state Open { url } in
    let elapsed = Chrome_api.performance_now () -. t0 in
    (match result with
     | Ok Protocol.Local ->
       debug state (Printf.sprintf "← Local (%.1f ms) %s" elapsed url)
     | Ok (Remote tid) ->
       debug state (Printf.sprintf "← Remote %s (%.1f ms) %s" tid elapsed url);
       Chrome_api.Tabs.remove tab_id
     | Error msg -> log (Printf.sprintf "Open error: %s" msg));
    Lwt.return state

let delete_matching_rule state url =
  let* result = call state Test { url } in
  match result with
  | Ok (Protocol.Match { rule_index; _ }) ->
    let* rules_result = call state Get_rules () in
    (match rules_result with
     | Ok existing ->
       let updated = List.filteri existing ~f:(fun i _ -> not (Int.equal i rule_index)) in
       let* _set_result = call state Set_rules updated in
       Lwt.return (Ok ())
     | Error msg -> Lwt.return (Error msg))
  | Ok (No_match _) -> Lwt.return (Error "No matching rule")
  | Error msg -> Lwt.return (Error msg)

let handle_context_menu state menu_id link_url page_url tab_id =
  let effective_url =
    match String.is_empty link_url with
    | true -> page_url
    | false -> link_url
  in
  match String.lsplit2 menu_id ~on:':' with
  | Some ("open_in", target) ->
    (match String.is_empty link_url with
     | true -> Lwt.return state
     | false ->
       let* _result = call state Open_on { target; url = link_url } in
       Lwt.return state)
  | Some ("send_to", target) ->
    (match String.is_empty page_url with
     | true -> Lwt.return state
     | false ->
       let* result = call state Open_on { target; url = page_url } in
       (match result with
        | Ok _ -> Option.iter tab_id ~f:Chrome_api.Tabs.remove
        | Error _ -> ());
       Lwt.return state)
  | _ when String.equal menu_id "add_rule" ->
    (match String.is_empty effective_url with
     | true -> Lwt.return state
     | false ->
       let encoded_url =
         effective_url |> Js.string |> Js.encodeURIComponent |> Js.to_string
       in
       let dialog_url = Printf.sprintf "add_rule.html?url=%s" encoded_url in
       Chrome_api.Windows.create_popup ~url:dialog_url
         ~width:Constants.popup_width ~height:Constants.popup_height;
       Lwt.return state)
  | _ when String.equal menu_id "delete_rule" ->
    (match String.is_empty effective_url with
     | true -> Lwt.return state
     | false ->
       let* result = delete_matching_rule state effective_url in
       (match result with
        | Ok () -> ()
        | Error msg -> log (Printf.sprintf "Delete rule: %s" msg));
       Lwt.return state)
  | _ -> Lwt.return state

let handle_local_action state json respond =
  match string_field json "action" with
  | Ok "reconnect" ->
    let state = connect state in
    respond (`Assoc [ ("connected", `Bool (is_connected state)) ]);
    Lwt.return state
  | Ok "delete_matching_rule" ->
    let url =
      match string_field json "url" with
      | Ok s -> s
      | Error _ -> ""
    in
    (match String.is_empty url with
     | true ->
       respond (`Assoc [ ("error", `String "url required") ]);
       Lwt.return state
     | false ->
       let* result = delete_matching_rule state url in
       (match result with
        | Ok () -> respond (`Assoc [ ("ok", `Bool true) ])
        | Error msg -> respond (`Assoc [ ("error", `String msg) ]));
       Lwt.return state)
  | Ok other ->
    log (Printf.sprintf "Unknown popup action: %s" other);
    respond (`Assoc [ ("error", `String "unknown action") ]);
    Lwt.return state
  | Error _ ->
    respond (`Assoc [ ("error", `String "invalid message") ]);
    Lwt.return state

let handle_popup_query state json respond =
  let parsed =
    Result.bind (Protocol.frame_of_yojson json) ~f:Protocol.parse_request_payload
  in
  match parsed with
  | Error _ -> handle_local_action state json respond
  | Ok rp ->
    match state.connection with
    | None ->
      respond (Protocol.frame_to_yojson (Protocol.make_response_frame 0 (Error "Not connected")));
      Lwt.return state
    | Some conn ->
      let* resp_frame = Client.send_raw_command conn ~command:rp.Protocol.command ~params:rp.params in
      respond (Protocol.frame_to_yojson resp_frame);
      Lwt.return state

let setup_context_menus tenants self_id =
  remove_all_context_menus (fun () ->
    create_context_menu ~id:"open_in" ~title:"Open link in" ~contexts:[ "link" ];
    create_context_menu ~id:"send_to" ~title:"Send page" ~contexts:[ "page" ];
    List.iter tenants ~f:(fun (tid, label, connected) ->
      let is_self = Option.exists self_id ~f:(String.equal tid) in
      let enabled = connected && not is_self in
      let title =
        match (is_self, connected) with
        | (true, _) -> Printf.sprintf "%s (this)" label
        | (false, false) -> Printf.sprintf "%s (offline)" label
        | (false, true) -> label
      in
      create_child_context_menu
        ~id:(Printf.sprintf "open_in:%s" tid) ~parent_id:"open_in"
        ~title ~contexts:[ "link" ] ~enabled ();
      create_child_context_menu
        ~id:(Printf.sprintf "send_to:%s" tid) ~parent_id:"send_to"
        ~title ~contexts:[ "page" ] ~enabled ());
    create_context_menu ~id:"add_rule" ~title:"Add rule" ~contexts:[ "page"; "link" ];
    create_context_menu ~id:"delete_rule" ~title:"Delete matching rule" ~contexts:[ "page"; "link" ])

let handle_event state event =
  match event with
  | Navigation { url; tab_id } -> handle_navigation state url tab_id
  | Bridge_event (Push p) -> Lwt.return (handle_push state p)
  | Bridge_event Disconnected ->
    log "Bridge connection lost";
    Lwt.return { state with connection = None }
  | Port_disconnected ->
    log "Native port disconnected, reconnecting in 2s…";
    Chrome_api.set_timeout (fun () -> push Connect_requested) Constants.reconnect_delay_ms;
    Lwt.return { initial_state with debug_logging = state.debug_logging }
  | Connect_requested -> Lwt.return (connect state)
  | Connect_with_settings { port; tenant_name; daemon_host; daemon_port; debug_logging } ->
    Lwt.return (connect_with_settings port tenant_name daemon_host daemon_port ~debug_logging)
  | Connection_ready conn ->
    log (Printf.sprintf "Registered as tenant: %s" (Client.tenant_name conn));
    (* Flush any queued subclient requests *)
    List.iter (List.rev state.subclient_queue) ~f:(fun (_coord_id, serialized) ->
      Client.subclient_write conn serialized);
    Lwt.async (fun () ->
      let stream = Client.subclient_read conn in
      let rec forward () =
        let* raw = Lwt_stream.next stream in
        push (Subclient_response raw);
        forward ()
      in
      Lwt.catch forward (fun _exn -> Lwt.return_unit));
    Lwt.return { state with connection = Some conn;
      self_tenant_id = Some (Client.tenant_name conn);
      subclient_queue = [] }
  | Context_menu { menu_id; link_url; page_url; tab_id } ->
    handle_context_menu state menu_id link_url page_url tab_id
  | Popup_query { json; respond } -> handle_popup_query state json respond
  | Setup_menus ->
    setup_context_menus state.tenant_names state.self_tenant_id;
    Lwt.return state
  | Refresh_menus { tenants } ->
    setup_context_menus tenants state.self_tenant_id;
    Lwt.return { state with tenant_names = tenants }
  | Page_port_message { port; raw = _; frame } ->
    begin match Multiplexer.is_register_frame frame with
    | true ->
      (* Registration handled locally — add port to broadcast list, send Registered back *)
      let desired =
        match Protocol.parse_request_payload frame with
        | Ok rp ->
          begin match Protocol.request_deserializer Protocol.Register rp.params with
          | Ok req -> Option.value req.name ~default:"anonymous"
          | Error _ -> "anonymous"
          end
        | Error _ -> "anonymous"
      in
      log (Printf.sprintf "Port registered: %s" desired);
      let registered_frame = Protocol.make_push_frame (Registered { tenant_id = desired }) in
      Chrome_api.Port.post_message_json port (Protocol.serialize_frame registered_frame);
      Lwt.return { state with
        subclient_ports = port :: state.subclient_ports }
    | false ->
      (* Request: assign coordinator ID, store mapping, forward or queue *)
      let coord_id = state.subclient_next_id in
      let wire_frame = { Protocol.id = coord_id; payload = frame.payload } in
      let serialized = Protocol.serialize_frame wire_frame in
      let state = { state with
        subclient_next_id = coord_id + 1;
        subclient_pending = Map.set state.subclient_pending ~key:coord_id
          ~data:(frame.id, port) } in
      match state.connection with
      | Some conn ->
        log (Printf.sprintf "Port request: orig_id=%d coord_id=%d" frame.id coord_id);
        Client.subclient_write conn serialized;
        Lwt.return state
      | None ->
        log (Printf.sprintf "Port request queued: orig_id=%d coord_id=%d" frame.id coord_id);
        Lwt.return { state with subclient_queue = (coord_id, serialized) :: state.subclient_queue }
    end
  | Page_port_disconnected { port } ->
    log "Port disconnected";
    Lwt.return { state with
      subclient_ports = List.filter state.subclient_ports ~f:(fun p ->
        not (phys_equal p port));
      subclient_pending = Map.filter state.subclient_pending ~f:(fun (_orig_id, p) ->
        not (phys_equal p port)) }
  | Subclient_response raw ->
    begin match Protocol.deserialize_frame raw with
    | Error msg ->
      log (Printf.sprintf "Subclient response parse error: %s" msg);
      Lwt.return state
    | Ok frame ->
      match frame.id with
      | 0 ->
        (* Broadcast push to all subclient ports *)
        List.iter state.subclient_ports ~f:(fun port ->
          Chrome_api.Port.post_message_json port raw);
        Lwt.return state
      | coord_id ->
        match Map.find state.subclient_pending coord_id with
        | Some (orig_id, port) ->
          let restored_frame = { Protocol.id = orig_id; payload = frame.payload } in
          Chrome_api.Port.post_message_json port (Protocol.serialize_frame restored_frame);
          Lwt.return { state with
            subclient_pending = Map.remove state.subclient_pending coord_id }
        | None ->
          log (Printf.sprintf "Subclient response: unknown coord_id=%d, dropped" coord_id);
          Lwt.return state
    end

(* -- Coordinator loop *)

let rec coordinator state =
  let* event = Lwt_stream.next event_stream in
  let* state = handle_event state event in
  update_badge (Option.is_some state.self_tenant_id);
  coordinator state

(* -- Chrome event registration *)

let register_chrome_listeners () =
  on_before_navigate (fun url tab_id frame_id ->
      match frame_id with
      | 0 -> push (Navigation { url; tab_id })
      | _ -> ());
  on_context_menu_clicked (fun menu_id link_url page_url tab_id ->
      push (Context_menu { menu_id; link_url; page_url; tab_id }));
  Chrome_api.Runtime.on_connect (fun port ->
    log "Port connected";
    Chrome_api.Port.on_message_json port (fun raw ->
      log (Printf.sprintf "Port raw message: %s"
        (String.prefix raw 200));
      match Protocol.deserialize_frame raw with
      | Error msg ->
        log (Printf.sprintf "Port frame parse error: %s" msg)
      | Ok frame ->
        push (Page_port_message { port; raw; frame }));
    Chrome_api.Port.on_disconnect port (fun () ->
      push (Page_port_disconnected { port })));
  Chrome_api.Runtime.on_message (fun msg_str respond ->
     match Protocol.parse_json_string msg_str with
     | Error _ ->
       respond (Protocol.json_to_string (`Assoc [ ("error", `String "invalid JSON") ]))
     | Ok json ->
       push
         (Popup_query
            { json;
              respond = (fun resp -> respond (Protocol.json_to_string resp))
            }));
  on_installed (fun () ->
    log "Extension installed";
    push Setup_menus);
  on_startup (fun () ->
    log "Browser started";
    push Setup_menus)

(* -- Initialization *)

let () =
  log "Alloy extension starting";
  update_badge false;
  register_chrome_listeners ();
  push Connect_requested;
  Lwt.async (fun () -> coordinator initial_state)
