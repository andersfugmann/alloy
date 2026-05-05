open! Base
open! Stdio
open Js_of_ocaml

let log = Chrome_api.log

(* -- JSON conversion *)

let json_of_string (s : string) : (Yojson.Safe.t, string) Result.t =
  Protocol.parse_json_string s

let json_to_string (json : Yojson.Safe.t) : string =
  Yojson.Safe.to_string json

let string_field (json : Yojson.Safe.t) (key : string) :
    (string, string) Result.t =
  match json with
  | `Assoc pairs ->
    (match List.Assoc.find pairs ~equal:String.equal key with
     | Some (`String s) -> Ok s
     | Some _ -> Error (Printf.sprintf "field %s: expected string" key)
     | None -> Error (Printf.sprintf "missing field: %s" key))
  | _ -> Error "expected JSON object"

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
  | Bridge_message of { raw : string }
  | Port_disconnected
  | Connect_requested
  | Connect_with_settings of { port : native_port; tenant_name : string; daemon_host : string; daemon_port : string; debug_logging : bool }
  | Context_menu of { menu_id : string; link_url : string; page_url : string; tab_id : int option }
  | Popup_query of { json : Yojson.Safe.t; respond : Yojson.Safe.t -> unit }
  | Setup_menus
  | Refresh_menus of { tenants : (string * string * bool) list }
  | Self_registered of { tenant_id : string }
  | Delete_rule_at of { index : int }

type state = {
  native_port : native_port option;
  next_id : int;
  pending : (Protocol.response_envelope -> unit) Map.M(Int).t;
  tenant_names : (string * string * bool) list;
  self_tenant_id : string option;
  debug_logging : bool;
}

(* -- Event stream *)

let (event_stream : event Lwt_stream.t), push_event =
  Lwt_stream.create ()

let push ev = push_event (Some ev)

(* -- State operations (pure) *)

let initial_state = { native_port = None; next_id = 1; pending = Map.empty (module Int); tenant_names = []; self_tenant_id = None; debug_logging = false }

let debug (state : state) (msg : string) : unit =
  match state.debug_logging with
  | true -> log msg
  | false -> ()

let is_connected (state : state) : bool =
  Option.is_some state.native_port

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

let send_envelope (state : state) (command : string) (params : Yojson.Safe.t)
    (on_response : Protocol.response_envelope -> unit) : state =
  match state.native_port with
  | None ->
    log "No native port connected";
    state
  | Some p ->
    let id = state.next_id in
    let env : Protocol.request_envelope = { id; command; params; tenant = None } in
    log (Printf.sprintf "-> %s id=%d" command id);
    Chrome_api.Port.post_message_json p (json_to_string (Protocol.request_envelope_to_yojson env));
    { state with
      next_id = id + 1;
      pending = Map.set state.pending ~key:id ~data:on_response }

let send_command : type req resp. state -> (req, resp) Protocol.command -> req ->
    ((resp, string) Result.t -> unit) -> state =
  fun state cmd params on_result ->
    let command = Protocol.command_name cmd in
    let params_json = Protocol.serialize_params cmd params in
    send_envelope state command params_json (fun resp_env ->
      match resp_env.success with
      | true -> on_result (Protocol.deserialize_response cmd resp_env.payload)
      | false -> on_result (Error (Option.value resp_env.error ~default:"unknown error")))

(* -- Connection management *)

let non_empty (s : string) : string option =
  match String.is_empty s with
  | true -> None
  | false -> Some s

let connect_with_settings (port : native_port) (tenant_name : string) (daemon_host : string) (daemon_port : string) ~(debug_logging : bool) : state =
  let brand = non_empty (Chrome_api.Navigator.get_browser_brand ()) in
  let name = non_empty tenant_name in
  let address =
    let h = Option.value (non_empty daemon_host) ~default:"127.0.0.1" in
    let p = Option.value (non_empty daemon_port) ~default:(Int.to_string Constants.default_port) in
    Some (Printf.sprintf "%s:%s" h p)
  in
  log (Printf.sprintf "Browser brand: %s, tenant: %s, address: %s"
    (Option.value brand ~default:"(none)")
    (Option.value name ~default:"(default)")
    (Option.value address ~default:"(default)"));
  let state = { native_port = Some port; next_id = 1; pending = Map.empty (module Int); tenant_names = []; self_tenant_id = None; debug_logging } in
  (* Register uses id=0: response handled by id=0 handler, not pending map *)
  let register_params : Protocol.register_params = { brand; address; name } in
  let register_env = Protocol.make_request_envelope Register register_params 0 name in
  log "→ Register id=0";
  Chrome_api.Port.post_message_json port (json_to_string (Protocol.request_envelope_to_yojson register_env));
  state

let connect (_state : state) : state =
  match
    let p = Chrome_api.Runtime.connect_native "alloy" in
    log "Connected to native messaging host";
    Chrome_api.Port.on_message_json p (fun msg ->
      push (Bridge_message { raw = msg }));
    Chrome_api.Port.on_disconnect p (fun () -> push Port_disconnected);
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
    (* native_port stays None until connect_with_settings — prevents race *)
    initial_state
  | exception exn ->
    log (Printf.sprintf "Failed to connect: %s" (Exn.to_string exn));
    initial_state

(* -- Event handlers (pure state transformers) *)

let handle_push (state : state) (p : Protocol.push) : state =
  match p with
  | Navigate { url } ->
    log (Printf.sprintf "Received NAVIGATE push: %s" url);
    create_tab url;
    state
  | Registered { tenant_id } ->
    log (Printf.sprintf "Re-registered as tenant: %s" tenant_id);
    push (Self_registered { tenant_id });
    state
  | Config_updated { config = cfg; registered_tenants } ->
    log (Printf.sprintf "Config push: %d tenants, %d registered"
      (List.length cfg.tenants) (List.length registered_tenants));
    let registered_set = Set.of_list (module String) registered_tenants in
    let tenants = List.map cfg.tenants ~f:(fun (id, tc) ->
      (id, tc.Protocol.label, Set.mem registered_set id)) in
    push (Refresh_menus { tenants });
    state

let handle_bridge_message (state : state) (raw : string) : state =
  match json_of_string raw with
  | Error msg ->
    log (Printf.sprintf "Failed to parse bridge JSON: %s" msg);
    state
  | Ok json ->
    (match Protocol.server_message_of_yojson json with
     | Ok (Push { id = _; push = p }) ->
       handle_push state p
     | Ok (Response resp_env) ->
       (match resp_env.id with
        | 0 ->
          (* id=0: registration response *)
          (match resp_env.success with
           | true ->
             let tenant_id =
               match resp_env.payload with
               | `String s -> s
               | _ -> "unknown"
             in
             log (Printf.sprintf "← Registered id=0: %s" tenant_id);
             push (Self_registered { tenant_id });
             state
           | false ->
             log (Printf.sprintf "← Registration error id=0: %s"
               (Option.value resp_env.error ~default:"unknown"));
             push Port_disconnected;
             { state with native_port = None; pending = Map.empty (module Int) })
        | id ->
          log (Printf.sprintf "← Response id=%d success=%b (pending: %d)"
            id resp_env.success (Map.length state.pending));
          (match Map.find state.pending id with
           | None ->
             log (Printf.sprintf "Orphan response for id=%d" id);
             state
           | Some cb ->
             cb resp_env;
             { state with pending = Map.remove state.pending id }))
     | Error msg ->
       log (Printf.sprintf "Failed to parse bridge message: %s" msg);
       state)

let handle_navigation (state : state) (url : string) (tab_id : int) : state =
  match is_connected state with
  | false -> state
  | true ->
    (match is_internal_url url with
     | true -> state
     | false ->
       let t0 = Chrome_api.performance_now () in
       debug state (Printf.sprintf "→ Open %s" url);
       send_command state Open { url } (fun result ->
           let elapsed = Chrome_api.performance_now () -. t0 in
           match result with
           | Ok Protocol.Local ->
             debug state (Printf.sprintf "← Local (%.1f ms) %s" elapsed url)
           | Ok (Remote tid) ->
             debug state (Printf.sprintf "← Remote %s (%.1f ms) %s" tid elapsed url);
             Chrome_api.Tabs.remove tab_id
           | Error msg -> log (Printf.sprintf "Open error: %s" msg)))

let handle_context_menu (state : state) (menu_id : string)
    (link_url : string) (page_url : string) (tab_id : int option) : state =
  match String.lsplit2 menu_id ~on:':' with
  | Some ("open_in", target) ->
    (match String.is_empty link_url with
     | true -> state
     | false ->
       send_command state Open_on { target; url = link_url }
         (fun _result -> ()))
  | Some ("send_to", target) ->
    (match String.is_empty page_url with
     | true -> state
     | false ->
       send_command state Open_on { target; url = page_url }
         (fun _result -> Option.iter tab_id ~f:Chrome_api.Tabs.remove))
  | _ ->
  let url =
    match String.is_empty link_url with
    | true -> page_url
    | false -> link_url
  in
  match menu_id with
  | "add_rule" ->
    (match String.is_empty url with
     | true -> state
     | false ->
       let encoded_url =
         url |> Js.string |> Js.encodeURIComponent |> Js.to_string
       in
       let dialog_url =
         Printf.sprintf "add_rule.html?url=%s" encoded_url
       in
       Chrome_api.Windows.create_popup ~url:dialog_url ~width:420 ~height:300;
       state)
  | "delete_rule" ->
    (match String.is_empty url with
     | true -> state
     | false ->
       send_command state Test { url } (fun result ->
           match result with
           | Ok (Protocol.Match { rule_index; _ }) ->
             push (Delete_rule_at { index = rule_index })
           | Ok (No_match _) ->
             log (Printf.sprintf "No rule matches %s" url)
           | Error msg ->
             log (Printf.sprintf "Test error: %s" msg)))
  | _ -> state

let handle_local_action (state : state) (json : Yojson.Safe.t)
    (respond : Yojson.Safe.t -> unit) : state =
  match string_field json "action" with
  | Ok "reconnect" ->
    let state = connect state in
    respond (`Assoc [ ("connected", `Bool (is_connected state)) ]);
    state
  | Ok "delete_matching_rule" ->
    let url =
      match string_field json "url" with
      | Ok s -> s
      | Error _ -> ""
    in
    (match String.is_empty url with
     | true ->
       respond (`Assoc [ ("error", `String "url required") ]);
       state
     | false ->
       send_command state Test { url } (fun result ->
           match result with
           | Ok (Protocol.Match { rule_index; _ }) ->
             push (Delete_rule_at { index = rule_index });
             respond (`Assoc [ ("ok", `Bool true) ])
           | Ok (No_match _) ->
             respond (`Assoc [ ("error", `String "No matching rule") ])
           | Error msg ->
             respond (`Assoc [ ("error", `String msg) ])))
  | Ok other ->
    log (Printf.sprintf "Unknown popup action: %s" other);
    respond (`Assoc [ ("error", `String "unknown action") ]);
    state
  | Error _ ->
    respond (`Assoc [ ("error", `String "invalid message") ]);
    state

let handle_popup_query (state : state) (json : Yojson.Safe.t)
    (respond : Yojson.Safe.t -> unit) : state =
  (* Protocol command passthrough: {"cmd": "<command>", "params": <json>} *)
  match string_field json "cmd" with
  | Error _ -> handle_local_action state json respond
  | Ok cmd_name ->
    let params_json =
      match Yojson.Safe.Util.member "params" json with
      | `Null -> `Assoc []
      | p -> p
    in
    match is_connected state with
    | false ->
      respond (`Assoc [ ("success", `Bool false); ("error", `String "Not connected") ]);
      state
    | true ->
      send_envelope state cmd_name params_json (fun resp_env ->
        respond (Protocol.response_envelope_to_yojson resp_env))

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

let handle_delete_rule_at (state : state) (index : int) : state =
  send_command state Get_rules () (fun result ->
      match result with
      | Ok existing ->
        let updated = List.filteri existing ~f:(fun i _ -> not (Int.equal i index)) in
        send_command state Set_rules updated (fun result ->
            match result with
            | Ok () ->
              log (Printf.sprintf "Deleted rule at index %d" index)
            | Error msg ->
              log (Printf.sprintf "Delete rule error: %s" msg))
        |> ignore
      | Error msg ->
        log (Printf.sprintf "Failed to fetch rules: %s" msg))

let handle_event (state : state) (event : event) : state =
  match event with
  | Navigation { url; tab_id } -> handle_navigation state url tab_id
  | Bridge_message { raw } -> handle_bridge_message state raw
  | Port_disconnected ->
    log "Native port disconnected, reconnecting in 2s…";
    Chrome_api.set_timeout (fun () -> push Connect_requested) 2000;
    { initial_state with debug_logging = state.debug_logging }
  | Connect_requested -> connect state
  | Connect_with_settings { port; tenant_name; daemon_host; daemon_port; debug_logging } ->
    connect_with_settings port tenant_name daemon_host daemon_port ~debug_logging
  | Context_menu { menu_id; link_url; page_url; tab_id } ->
    handle_context_menu state menu_id link_url page_url tab_id
  | Popup_query { json; respond } -> handle_popup_query state json respond
  | Setup_menus ->
    setup_context_menus state.tenant_names state.self_tenant_id;
    state
  | Refresh_menus { tenants } ->
    setup_context_menus tenants state.self_tenant_id;
    { state with tenant_names = tenants }
  | Self_registered { tenant_id } ->
    log (Printf.sprintf "Registered as tenant: %s" tenant_id);
    { state with self_tenant_id = Some tenant_id }
  | Delete_rule_at { index } ->
    handle_delete_rule_at state index

(* -- Coordinator loop *)

let rec coordinator (state : state) : unit Lwt.t =
  let%lwt event = Lwt_stream.next event_stream in
  let state = handle_event state event in
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
     match json_of_string msg_str with
     | Error _ ->
       respond (json_to_string (`Assoc [ ("error", `String "invalid JSON") ]))
     | Ok json ->
       push
         (Popup_query
            { json;
              respond = (fun resp -> respond (json_to_string resp))
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
