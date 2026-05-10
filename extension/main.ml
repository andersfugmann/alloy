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

(* -- Shared state *)

let client_ref : Client.t option ref = ref None
let debug_ref = ref false
let status_codes : int Map.M(Int).t ref = ref (Map.empty (module Int))

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

let debug msg =
  match !debug_ref with
  | true -> log msg
  | false -> ()

let call : type req resp. (req, resp) Protocol.command -> req ->
    (resp, string) Result.t Lwt.t =
  fun cmd request ->
    match !client_ref with
    | None -> Lwt.return (Error "not connected")
    | Some conn ->
      log (Printf.sprintf "-> %s" (Protocol.command_name cmd));
      Client.call conn cmd request

(* -- Helpers *)

let non_empty s =
  match String.is_empty s with
  | true -> None
  | false -> Some s

(* -- Context menu management *)

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

(* -- Rule management *)

let delete_matching_rule url =
  let* result = call Test { url; title = None } in
  match result with
  | Ok (Protocol.Match { rule_index; _ }) ->
    let* rules_result = call Get_rules () in
    (match rules_result with
     | Ok existing ->
       let updated = List.filteri existing ~f:(fun i _ -> not (Int.equal i rule_index)) in
       let* _set_result = call Set_rules updated in
       Lwt.return (Ok ())
     | Error msg -> Lwt.return (Error msg))
  | Ok (No_match _) -> Lwt.return (Error "No matching rule")
  | Error msg -> Lwt.return (Error msg)

(* -- Push handlers (registered via Client.register_broadcast) *)

let handle_broadcast push_opt =
  match push_opt with
  | Some (Protocol.Navigate url) ->
    log (Printf.sprintf "Received NAVIGATE push: %s" url);
    create_tab url
  | Some (Config_updated { config = cfg; registered_tenants }) ->
    log (Printf.sprintf "Config push: %d tenants, %d registered"
      (List.length cfg.tenants) (List.length registered_tenants));
    let registered_set = Set.of_list (module String) registered_tenants in
    let tenants = List.map cfg.tenants ~f:(fun (id, tc) ->
      (id, tc.Protocol.label, Set.mem registered_set id)) in
    let self_id = Option.map !client_ref ~f:Client.name in
    setup_context_menus tenants self_id
  | None -> ()

(* -- Chrome event handlers (direct callbacks) *)

let handle_navigation url tab_id =
  status_codes := Map.remove !status_codes tab_id;
  match Option.is_some !client_ref && not (is_internal_url url) with
  | false -> ()
  | true ->
    Lwt.async (fun () ->
      let t0 = Chrome_api.performance_now () in
      debug (Printf.sprintf "→ Open %s" url);
      let* result = call Open { url; title = None } in
      let elapsed = Chrome_api.performance_now () -. t0 in
      (match result with
       | Ok Protocol.Local ->
         debug (Printf.sprintf "← Local (%.1f ms) %s" elapsed url)
       | Ok (Remote tid) ->
         debug (Printf.sprintf "← Remote %s (%.1f ms) %s" tid elapsed url);
         Chrome_api.Tabs.remove tab_id
       | Error msg -> log (Printf.sprintf "Open error: %s" msg));
      Lwt.return_unit)

let handle_request_completed _url tab_id status_code =
  status_codes := Map.set !status_codes ~key:tab_id ~data:status_code

let handle_page_completed url tab_id frame_id =
  match frame_id with
  | 0 ->
    let status = Map.find !status_codes tab_id in
    status_codes := Map.remove !status_codes tab_id;
    let is_error =
      match status with
      | None -> true
      | Some code -> code >= 400
    in
    (match Option.is_some !client_ref && not (is_internal_url url) && not is_error with
     | false -> ()
     | true ->
       Lwt.async (fun () ->
         let (title_promise, title_resolver) = Lwt.wait () in
         Chrome_api.Tabs.get_title tab_id ~on_result:(fun title ->
           Lwt.wakeup_later title_resolver title);
         let* title = title_promise in
         debug (Printf.sprintf "→ Page_loaded %s (title: %s)" url
           (Option.value title ~default:"<none>"));
         let* _result = call Page_loaded { url; title } in
         Lwt.return_unit))
  | _ -> ()

let handle_context_menu menu_id link_url page_url tab_id =
  let effective_url =
    match String.is_empty link_url with
    | true -> page_url
    | false -> link_url
  in
  Lwt.async (fun () ->
    match String.lsplit2 menu_id ~on:':' with
    | Some ("open_in", target) ->
      (match String.is_empty link_url with
       | true -> Lwt.return_unit
       | false ->
         let* _result = call Open_on { target; url = link_url; title = None } in
         Lwt.return_unit)
    | Some ("send_to", target) ->
      (match String.is_empty page_url with
       | true -> Lwt.return_unit
       | false ->
         let* result = call Open_on { target; url = page_url; title = None } in
         (match result with
          | Ok _ -> Option.iter tab_id ~f:Chrome_api.Tabs.remove
          | Error _ -> ());
         Lwt.return_unit)
    | _ when String.equal menu_id "add_rule" ->
      (match String.is_empty effective_url with
       | true -> Lwt.return_unit
       | false ->
         let encoded_url =
           effective_url |> Js.string |> Js.encodeURIComponent |> Js.to_string
         in
         let dialog_url = Printf.sprintf "add_rule.html?url=%s" encoded_url in
         Chrome_api.Windows.create_popup ~url:dialog_url
           ~width:Constants.popup_width ~height:Constants.popup_height;
         Lwt.return_unit)
    | _ when String.equal menu_id "delete_rule" ->
      (match String.is_empty effective_url with
       | true -> Lwt.return_unit
       | false ->
         let* result = delete_matching_rule effective_url in
         (match result with
          | Ok () -> ()
          | Error msg -> log (Printf.sprintf "Delete rule: %s" msg));
         Lwt.return_unit)
    | _ -> Lwt.return_unit)

(* -- Connection management *)

let mux = Multiplexer.init ()

let rec connect () =
  Chrome_api.Storage.get_local [ "tenant_name"; "daemon_host"; "daemon_port"; "debug_logging" ]
    ~on_result:(fun pairs ->
      let find k =
        List.Assoc.find pairs ~equal:String.equal k
        |> Option.value ~default:""
      in
      connect_with_settings
        (find "tenant_name")
        (find "daemon_host")
        (find "daemon_port")
        ~debug_logging:(String.equal (find "debug_logging") "true"))

and connect_with_settings tenant_name daemon_host daemon_port ~debug_logging =
  debug_ref := debug_logging;
  let brand = non_empty (Chrome_api.Navigator.get_browser_brand ()) in
  let name = non_empty tenant_name in
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
  Lwt.async (fun () ->
    Lwt.catch
      (fun () ->
        let* (client, _push_stream) =
          Bridge.make_client ?name ?brand ~host ~port:port_num ~debug:debug_logging ()
        in
        log (Printf.sprintf "Registered as tenant: %s" (Client.name client));
        client_ref := Some client;
        update_badge true;
        Multiplexer.start mux client;
        Client.register_broadcast client handle_broadcast;
        let* () = Client.closed client in
        handle_disconnect ();
        Lwt.return_unit)
      (fun exn ->
        log (Printf.sprintf "Bridge connection failed: %s" (Exn.to_string exn));
        handle_disconnect ();
        Lwt.return_unit))

and handle_disconnect () =
  log "Bridge connection lost, reconnecting in 2s…";
  client_ref := None;
  update_badge false;
  setup_context_menus [] None;
  Chrome_api.set_timeout (fun () -> connect ()) Constants.reconnect_delay_ms

(* -- Chrome event registration *)

let register_chrome_listeners () =
  on_before_navigate (fun url tab_id frame_id ->
      match frame_id with
      | 0 -> handle_navigation url tab_id
      | _ -> ());
  Chrome_api.Web_request.on_completed (fun url tab_id status_code ->
    handle_request_completed url tab_id status_code);
  Chrome_api.Web_navigation.on_completed (fun url tab_id frame_id ->
    handle_page_completed url tab_id frame_id);
  on_context_menu_clicked (fun menu_id link_url page_url tab_id ->
      handle_context_menu menu_id link_url page_url tab_id);
  on_installed (fun () ->
    log "Extension installed";
    setup_context_menus [] None);
  on_startup (fun () ->
    log "Browser started";
    setup_context_menus [] None);
  Chrome_api.Commands.on_command (fun cmd ->
    match String.equal cmd "open-history" with
    | true ->
      Chrome_api.Windows.get_last_focused ~on_result:(fun window_id ->
        Chrome_api.Side_panel.open_panel ~window_id)
    | false -> ())

(* -- Initialization *)

let () =
  log "Alloy extension starting";
  update_badge false;
  register_chrome_listeners ();
  connect ()
