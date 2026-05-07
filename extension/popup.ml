open! Base
open! Stdio
open Js_of_ocaml

let ( let* ) = Lwt.bind

(* -- DOM elements -- *)

let status_dot = Page_util.get_by_id "statusDot"
let status_text = Page_util.get_by_id "statusText"
let tenant_list = Page_util.get_by_id "tenantList"
let footer = Page_util.get_by_id "footer"
let btn_add_rule = Page_util.get_by_id "btnAddRule"
let btn_delete_rule = Page_util.get_by_id "btnDeleteRule"

(* Track whether the active tab is actionable *)
let page_is_internal = ref true

(* -- Helpers -- *)

let set_status (connected : bool) (text : string) : unit =
  Page_util.set_class status_dot
    (match connected with
     | true -> "dot connected"
     | false -> "dot disconnected");
  Page_util.set_text status_text text

let set_footer ?(cls = "") (text : string) : unit =
  Page_util.set_class footer cls;
  Page_util.set_text footer text

(* -- Send page to tenant -- *)

let send_page_to (conn : Client.connection) (tenant : string) : unit =
  Page_util.query_active_tab ~on_result:(fun url tab_id ->
    Lwt.async (fun () ->
      let* result = Client.call conn Open_on { target = tenant; url } in
      begin match result with
      | Ok _ ->
        Chrome_api.Tabs.remove tab_id;
        Dom_html.window##close
      | Error msg ->
        set_footer ~cls:"error" (Printf.sprintf "Error: %s" msg)
      end;
      Lwt.return_unit))

(* -- Render tenants -- *)

let render_tenants (conn : Client.connection) (status : Protocol.status_info) (cfg : Protocol.config) (self_id : string) : unit =
  let registered_set =
    Set.of_list (module String) status.registered_tenants
  in
  let config_tenants =
    List.map cfg.tenants ~f:(fun (id, tc) ->
      (id, tc.Protocol.label, Set.mem registered_set id))
  in
  let config_ids =
    List.map config_tenants ~f:(fun (id, _, _) -> id)
    |> Set.of_list (module String)
  in
  let extra =
    Set.diff registered_set config_ids
    |> Set.to_list
    |> List.map ~f:(fun id -> (id, "", true))
  in
  let all_tenants =
    config_tenants @ extra
    |> List.sort ~compare:(fun (a, _, _) (b, _, _) -> String.compare a b)
  in

  Page_util.set_html tenant_list "";
  let doc = Dom_html.document in
  match List.is_empty all_tenants with
  | true ->
    Page_util.set_html tenant_list
      {|<li style="color:#5f6368">No tenants</li>|}
  | false ->
    List.iter all_tenants ~f:(fun (id, label, is_connected) ->
      let li = Dom_html.createLi doc in

      let dot = Dom_html.createSpan doc in
      Page_util.set_class (dot :> Dom_html.element Js.t)
        (match is_connected with
         | true -> "dot connected"
         | false -> "dot disconnected");
      Dom.appendChild li dot;

      let name_span = Dom_html.createSpan doc in
      Page_util.set_class (name_span :> Dom_html.element Js.t) "tenant-name";
      Page_util.set_text (name_span :> Dom_html.element Js.t) id;
      Dom.appendChild li name_span;

      (match String.equal id self_id with
       | false -> ()
       | true ->
         let you = Dom_html.createSpan doc in
         Page_util.set_class (you :> Dom_html.element Js.t) "tenant-label";
         Page_util.set_text (you :> Dom_html.element Js.t) "(this)";
         Dom.appendChild li you);

      (match String.is_empty label with
       | true -> ()
       | false ->
         let lbl = Dom_html.createSpan doc in
         Page_util.set_class (lbl :> Dom_html.element Js.t) "tenant-label";
         Page_util.set_text (lbl :> Dom_html.element Js.t)
           (Printf.sprintf "(%s)" label);
         Dom.appendChild li lbl);

      (* Send-to button — skip for self tenant *)
      (match is_connected && not (String.equal id self_id) with
       | false -> ()
       | true ->
         let btn = Dom_html.createButton doc in
         Page_util.set_class (btn :> Dom_html.element Js.t) "tenant-send";
         Page_util.set_text (btn :> Dom_html.element Js.t) "Send ↗";
         Page_util.set_disabled (btn :> Dom_html.element Js.t) !page_is_internal;
         Page_util.on_click (btn :> Dom_html.element Js.t) (fun () ->
           send_page_to conn id);
         Dom.appendChild li btn);

      Dom.appendChild tenant_list li)

(* -- Load tenants on popup open -- *)

let load_tenants (conn : Client.connection) : unit =
  Page_util.query_active_tab ~on_result:(fun url _tab_id ->
    let is_internal = Page_util.is_internal_url url in
    page_is_internal := is_internal;
    Page_util.set_disabled btn_add_rule is_internal;
    Page_util.set_disabled btn_delete_rule is_internal;
    Lwt.async (fun () ->
      let* status_result = Client.call conn Status () in
      let* config_result = Client.call conn Get_config () in
      let* info_result = Client.call conn Connection_info () in
      let self_id =
        match info_result with
        | Ok info -> info.Protocol.tenant_id
        | Error _ -> Client.tenant_name conn
      in
      begin match (status_result, config_result) with
      | (Ok status, Ok cfg) ->
        set_status (not (List.is_empty status.Protocol.registered_tenants))
          (match List.is_empty status.registered_tenants with
           | true -> "Disconnected"
           | false ->
             match String.is_empty self_id with
             | true -> Printf.sprintf "Connected (%d tenants)" (List.length status.registered_tenants)
             | false -> Printf.sprintf "Connected as %s" self_id);
        render_tenants conn status cfg self_id
      | _ ->
        set_status false "Disconnected";
        Page_util.set_html tenant_list
          {|<li style="color:#5f6368">Not connected</li>|}
      end;
      Lwt.return_unit))

(* -- Entry point -- *)

let () =
  Chrome_api.log "Popup script starting";
  Page_util.connect_port ~name:"popup"
    ~on_ready:(fun conn ->
      load_tenants conn;

      (* Add routing rule button *)
      Page_util.on_click btn_add_rule (fun () ->
        Page_util.query_active_tab ~on_result:(fun url _tab_id ->
          let encoded_url =
            url |> Js.string |> Js.encodeURIComponent |> Js.to_string
          in
          let dialog_url =
            Printf.sprintf "add_rule.html?url=%s" encoded_url
          in
          Chrome_api.Windows.create_popup ~url:(Page_util.get_extension_url dialog_url)
            ~width:420 ~height:300;
          Dom_html.window##close));

      (* Delete matching rule button *)
      Page_util.on_click btn_delete_rule (fun () ->
        Page_util.query_active_tab ~on_result:(fun url _tab_id ->
          Page_util.send_message
            (`Assoc [ ("action", `String "delete_matching_rule");
                      ("url", `String url) ])
            ~on_response:(fun result ->
              match result with
              | Ok json ->
                begin match Yojson.Safe.Util.member "ok" json with
                | `Bool true -> set_footer ~cls:"success" "Rule deleted"
                | _ ->
                  let msg = Yojson.Safe.Util.member "error" json in
                  begin match msg with
                  | `String s -> set_footer ~cls:"error" s
                  | _ -> set_footer ~cls:"error" "Unknown error"
                  end
                end
              | Error msg -> set_footer ~cls:"error" msg)));

      (* Configure button *)
      Page_util.on_click (Page_util.get_by_id "btnConfig") (fun () ->
        Page_util.create_tab (Page_util.get_extension_url "config.html");
        Dom_html.window##close);

      (* Reconnect button *)
      Page_util.on_click (Page_util.get_by_id "btnReconnect") (fun () ->
        Page_util.send_message
          (`Assoc [ ("action", `String "reconnect") ])
          ~on_response:(fun result ->
            match result with
            | Error _ -> set_status false "Error reconnecting"
            | Ok json ->
              match Yojson.Safe.Util.member "connected" json with
              | `Bool true -> set_status true "Reconnected"
              | _ -> set_status false "Disconnected")))
    ~on_event:(fun _ev -> ())
