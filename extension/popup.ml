open! Base
open! Stdio
open Js_of_ocaml

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

let send_page_to (tenant : string) : unit =
  Page_util.query_active_tab ~on_result:(fun url tab_id ->
    Page_util.send_protocol_command Open_on { target = tenant; url }
      ~on_response:(fun result ->
        match result with
        | Ok _ ->
          Chrome_api.Tabs.remove tab_id;
          Dom_html.window##close
        | Error msg ->
          set_footer ~cls:"error" (Printf.sprintf "Error: %s" msg)))

(* -- Render tenants -- *)

let render_tenants (status : Protocol.status_info) (cfg : Protocol.config) (self_id : string) : unit =
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
           send_page_to id);
         Dom.appendChild li btn);

      Dom.appendChild tenant_list li)

(* -- Load tenants on popup open -- *)

let () =
  Page_util.query_active_tab ~on_result:(fun url _tab_id ->
    let is_internal = Page_util.is_internal_url url in
    page_is_internal := is_internal;
    Page_util.set_disabled btn_add_rule is_internal;
    Page_util.set_disabled btn_delete_rule is_internal;
    let status_ref = ref None in
    let config_ref = ref None in
    let self_id_ref = ref "" in
    let pending = ref 3 in
    let try_render () =
      match !pending > 0 with
      | true -> ()
      | false ->
        match (!status_ref, !config_ref) with
        | (Some status, Some cfg) ->
          let self = !self_id_ref in
          set_status (not (List.is_empty status.Protocol.registered_tenants))
            (match List.is_empty status.registered_tenants with
             | true -> "Disconnected"
             | false ->
               match String.is_empty self with
               | true -> Printf.sprintf "Connected (%d tenants)" (List.length status.registered_tenants)
               | false -> Printf.sprintf "Connected as %s" self);
          render_tenants status cfg self
        | _ ->
          set_status false "Disconnected";
          Page_util.set_html tenant_list
            {|<li style="color:#5f6368">Not connected</li>|}
    in
    Page_util.storage_get [ "tenant_name" ] ~on_result:(fun pairs ->
      self_id_ref :=
        (List.Assoc.find pairs ~equal:String.equal "tenant_name"
         |> Option.value ~default:"");
      pending := !pending - 1;
      try_render ());
    Page_util.send_protocol_command Status ()
      ~on_response:(fun result ->
        (match result with
         | Ok info -> status_ref := Some info
         | Error _ -> ());
        pending := !pending - 1;
        try_render ());
    Page_util.send_protocol_command Get_config ()
      ~on_response:(fun result ->
        (match result with
         | Ok cfg -> config_ref := Some cfg
         | Error _ -> ());
        pending := !pending - 1;
        try_render ()))

(* -- Add routing rule button -- *)

let () =
  Page_util.on_click (Page_util.get_by_id "btnAddRule") (fun () ->
    Page_util.query_active_tab ~on_result:(fun url _tab_id ->
      let encoded_url =
        url |> Js.string |> Js.encodeURIComponent |> Js.to_string
      in
      let dialog_url =
        Printf.sprintf "add_rule.html?url=%s" encoded_url
      in
      Chrome_api.Windows.create_popup ~url:(Page_util.get_extension_url dialog_url)
        ~width:420 ~height:300;
      Dom_html.window##close))

(* -- Delete matching rule button -- *)

let () =
  Page_util.on_click (Page_util.get_by_id "btnDeleteRule") (fun () ->
    Page_util.query_active_tab ~on_result:(fun url _tab_id ->
      Page_util.send_message
        (`Assoc [ ("action", `String "delete_matching_rule");
                  ("url", `String url) ])
        ~on_response:(fun result ->
          match result with
          | Ok json ->
            (match Yojson.Safe.Util.member "ok" json with
             | `Bool true -> set_footer ~cls:"success" "Rule deleted"
             | _ ->
               let msg = Yojson.Safe.Util.member "error" json in
               match msg with
               | `String s -> set_footer ~cls:"error" s
               | _ -> set_footer ~cls:"error" "Unknown error")
          | Error msg -> set_footer ~cls:"error" msg)))

(* -- Configure button -- *)

let () =
  Page_util.on_click (Page_util.get_by_id "btnConfig") (fun () ->
    Page_util.create_tab (Page_util.get_extension_url "config.html");
    Dom_html.window##close)

(* -- Reconnect button -- *)

let () =
  Page_util.on_click (Page_util.get_by_id "btnReconnect") (fun () ->
    Page_util.send_message
      (`Assoc [ ("action", `String "reconnect") ])
      ~on_response:(fun result ->
        match result with
        | Error _ -> set_status false "Error reconnecting"
        | Ok json ->
          match Yojson.Safe.Util.member "connected" json with
          | `Bool true -> set_status true "Reconnected"
          | _ -> set_status false "Disconnected"))
