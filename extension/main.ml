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

type state = {
  connection : Client.connection option;
  tenant_names : (string * string * bool) list;
  self_tenant_id : string option;
  debug_logging : bool;
}

(* -- Event stream *)

let (event_stream : event Lwt_stream.t), push_event =
  Lwt_stream.create ()

let push ev = push_event (Some ev)

(* -- State operations *)

let initial_state = { connection = None; tenant_names = []; self_tenant_id = None; debug_logging = false }

let debug (state : state) (msg : string) : unit =
  match state.debug_logging with
  | true -> log msg
  | false -> ()

let is_connected (state : state) : bool =
  Option.is_some state.connection

let update_badge (connected : bool) : unit =
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

let string_field (json : Yojson.Safe.t) (key : string) : (string, string) Result.t =
  match json with
  | `Assoc pairs ->
    (match List.Assoc.find pairs ~equal:String.equal key with
     | Some (`String s) -> Ok s
     | Some _ -> Error (Printf.sprintf "field %s: expected string" key)
     | None -> Error (Printf.sprintf "missing field: %s" key))
  | _ -> Error "expected JSON object"

(* -- Connection management *)

let non_empty (s : string) : string option =
  match String.is_empty s with
  | true -> None
  | false -> Some s

let connect_with_settings (port : native_port) (tenant_name : string) (_daemon_host : string) (_daemon_port : string) ~(debug_logging : bool) : state =
  let brand = non_empty (Chrome_api.Navigator.get_browser_brand ()) in
  let name = non_empty tenant_name in
  log (Printf.sprintf "Browser brand: %s, tenant: %s"
    (Option.value brand ~default:"(none)")
    (Option.value name ~default:"(default)"));
  let write msg = Chrome_api.Port.post_message_json port msg in
  let (read, push_incoming) = Lwt_stream.create () in
  Chrome_api.Port.on_message_json port (fun msg -> push_incoming (Some msg));
  Chrome_api.Port.on_disconnect port (fun () -> push Port_disconnected);
  (* Client.init is async — sends register and waits for Registered push *)
  Lwt.async (fun () ->
    let* (conn, bridge_events) = Client.init ~write ~read ?tenant:name ?name ?brand () in
    push (Connection_ready conn);
    let rec forward () =
      let* ev = Lwt_stream.next bridge_events in
      push (Bridge_event ev);
      forward ()
    in
    Lwt.catch forward (fun _exn -> Lwt.return_unit));
  { connection = None; tenant_names = []; self_tenant_id = None; debug_logging }

let connect (_state : state) : state =
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

let handle_push (state : state) (p : Protocol.push) : state =
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

let handle_navigation (state : state) (url : string) (tab_id : int) : state Lwt.t =
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

let delete_matching_rule (state : state) (url : string)
    : (unit, string) Result.t Lwt.t =
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

let handle_context_menu (state : state) (menu_id : string)
    (link_url : string) (page_url : string) (tab_id : int option) : state Lwt.t =
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

let handle_local_action (state : state) (json : Yojson.Safe.t)
    (respond : Yojson.Safe.t -> unit) : state Lwt.t =
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

let handle_popup_query (state : state) (json : Yojson.Safe.t)
    (respond : Yojson.Safe.t -> unit) : state Lwt.t =
  match string_field json "cmd" with
  | Error _ -> handle_local_action state json respond
  | Ok cmd_name ->
    let params_json =
      match Yojson.Safe.Util.member "params" json with
      | `Null -> `Assoc []
      | p -> p
    in
    match state.connection with
    | None ->
      respond (`Assoc [ ("success", `Bool false); ("error", `String "Not connected") ]);
      Lwt.return state
    | Some conn ->
      let* resp_env = Client.send_raw_command conn ~command:cmd_name ~params:params_json in
      respond (Protocol.response_envelope_to_yojson resp_env);
      Lwt.return state

let setup_context_menus (tenants : (string * string * bool) list) (self_id : string option) : unit =
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

let handle_event (state : state) (event : event) : state Lwt.t =
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
    Lwt.return { state with connection = Some conn; self_tenant_id = Some (Client.tenant_name conn) }
  | Context_menu { menu_id; link_url; page_url; tab_id } ->
    handle_context_menu state menu_id link_url page_url tab_id
  | Popup_query { json; respond } -> handle_popup_query state json respond
  | Setup_menus ->
    setup_context_menus state.tenant_names state.self_tenant_id;
    Lwt.return state
  | Refresh_menus { tenants } ->
    setup_context_menus tenants state.self_tenant_id;
    Lwt.return { state with tenant_names = tenants }

(* -- Coordinator loop *)

let rec coordinator (state : state) : unit Lwt.t =
  let* event = Lwt_stream.next event_stream in
  let* state = handle_event state event in
  update_badge (Option.is_some state.self_tenant_id);
  coordinator state

(* -- Chrome event registration *)

let register_chrome_listeners () : unit =
  on_before_navigate (fun url tab_id frame_id ->
      match frame_id with
      | 0 -> push (Navigation { url; tab_id })
      | _ -> ());
  on_context_menu_clicked (fun menu_id link_url page_url tab_id ->
      push (Context_menu { menu_id; link_url; page_url; tab_id }));
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
