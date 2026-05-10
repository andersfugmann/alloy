open! Base
open! Stdio
open Js_of_ocaml

let ( let* ) = Lwt.bind

let rec swap_nth n = function
  | a :: b :: rest when n = 0 -> b :: a :: rest
  | x :: rest -> x :: swap_nth (n - 1) rest
  | l -> l

(* -- Connection -- *)

let conn_ref : Client.t option ref = ref None

let send_command : type req resp. (req, resp) Protocol.command -> req ->
    on_response:((resp, string) Result.t -> unit) -> unit =
  fun cmd params ~on_response ->
    match !conn_ref with
    | None -> on_response (Error "not connected")
    | Some conn ->
      Lwt.async (fun () ->
        let* result = Client.call conn cmd params in
        on_response result;
        Lwt.return_unit)

(* -- Mutable state (required for async UI callbacks) -- *)

let config : Protocol.config ref =
  ref
    Protocol.{
      listen = Constants.default_listen;
      allowed_networks = Constants.default_allowed_networks;
      tenants = [];
      defaults =
        { unmatched = "local"; cooldown_seconds = 5; browser_launch_timeout = 10 };
      history_exclude_patterns = [];
    }

let connected_tenants : Set.M(String).t ref =
  ref (Set.empty (module String))

let editing_tenant_id : string option ref = ref None
let editing_rule_index : int option ref = ref None

(* -- DOM elements -- *)

let status_dot = Page_util.get_by_id "statusDot"
let status_text = Page_util.get_by_id "statusText"
let loading_el = Page_util.get_by_id "loading"
let content_el = Page_util.get_by_id "content"
let footer_msg = Page_util.get_by_id "footerMsg"
let tenant_list_el = Page_util.get_by_id "tenantList"
let rule_list_el = Page_util.get_by_id "ruleList"
let tenant_form_el = Page_util.get_by_id "tenantForm"
let rule_form_el = Page_util.get_by_id "ruleForm"

(* -- Helpers -- *)

let set_status connected =
  let cls = match connected with true -> "ok" | false -> "err" in
  let badge_cls =
    match connected with true -> "connected" | false -> "disconnected"
  in
  Page_util.set_class status_dot (Printf.sprintf "dot %s" cls);
  Page_util.set_text status_text
    (match connected with true -> "Connected" | false -> "Disconnected");
  (match Dom_html.getElementById_opt "statusBadge" with
   | Some badge ->
     Page_util.set_class badge (Printf.sprintf "status-badge %s" badge_cls)
   | None ->
     let parent = status_dot##.parentNode in
     Js.Opt.iter parent (fun p ->
       Js.Opt.iter (Dom_html.CoerceTo.element p) (fun el ->
         Page_util.set_class el
           (Printf.sprintf "status-badge %s" badge_cls))))

let show_msg text msg_type =
  Page_util.set_text footer_msg text;
  Page_util.set_class footer_msg (Printf.sprintf "msg %s" msg_type);
  match String.equal msg_type "success" with
  | true ->
    Page_util.set_timeout (fun () -> Page_util.set_text footer_msg "") 3000
  | false -> ()

let browser_cmd_from_brand brand =
  match brand with
  | None -> ""
  | Some b ->
    let lower = String.lowercase b in
    (match String.is_substring lower ~substring:"edge" with
     | true -> "microsoft-edge"
     | false ->
       (match String.is_substring lower ~substring:"chromium" with
        | true -> "chromium"
        | false ->
          (match String.is_substring lower ~substring:"chrome" with
           | true -> "chrome"
           | false -> "")))

(* -- Populate select dropdowns -- *)

let populate_rule_target selected =
  let sel = Page_util.select_by_id "rfTarget" in
  Page_util.set_html (sel :> Dom_html.element Js.t) "";
  let doc = Dom_html.document in
  List.iter !config.tenants ~f:(fun (id, tc) ->
    let opt =
      Page_util.create_option doc ~value:id
        ~text:(match String.is_empty tc.label with true -> id | false -> tc.label)
        ~selected:(String.equal id selected)
    in
    Dom.appendChild sel opt)

let populate_tenant_selects () =
  let unmatched_sel = Page_util.select_by_id "dfUnmatched" in
  let current_val =
    let v = Js.to_string unmatched_sel##.value in
    match String.is_empty v with
    | true -> !config.defaults.unmatched
    | false -> v
  in
  Page_util.set_html (unmatched_sel :> Dom_html.element Js.t) "";
  let doc = Dom_html.document in
  let local_opt =
    Page_util.create_option doc ~value:"local"
      ~text:"Local (no rerouting)"
      ~selected:(String.equal current_val "local")
  in
  Dom.appendChild unmatched_sel local_opt;
  List.iter !config.tenants ~f:(fun (id, tc) ->
    let opt =
      Page_util.create_option doc ~value:id
        ~text:(match String.is_empty tc.label with true -> id | false -> tc.label)
        ~selected:(String.equal id current_val)
    in
    Dom.appendChild unmatched_sel opt);
  populate_rule_target (Js.to_string (Page_util.select_by_id "rfTarget")##.value)

(* -- Tenant CRUD -- *)

let rec render_tenants () =
  let tenants = !config.tenants in
  match List.is_empty tenants with
  | true ->
    Page_util.set_html tenant_list_el
      {|<div class="card-empty">No tenants defined. Add a tenant to configure browser profiles.</div>|}
  | false ->
    let html =
      List.map tenants ~f:(fun (id, t) ->
        let is_connected = Set.mem !connected_tenants id in
        let dot_class =
          match is_connected with true -> "connected" | false -> "disconnected"
        in
        let dot_title =
          match is_connected with true -> "Connected" | false -> "Disconnected"
        in
        let brand_html =
          match t.brand with
          | Some b when not (String.is_empty b) ->
            Printf.sprintf
              {| <span class="tenant-brand">(%s)</span>|}
              (Page_util.escape_html b)
          | _ -> ""
        in
        let cmd_html =
          match t.browser_cmd with
          | Some cmd -> Page_util.escape_html cmd
          | None ->
            {|<span style="color:#5f6368;font-style:italic">no launch command</span>|}
        in
        let cmd_title =
          match t.browser_cmd with
          | Some cmd -> Page_util.escape_html cmd
          | None ->
            Page_util.escape_html
              "No launch command \u{2014} browser will not be started automatically"
        in
        Printf.sprintf
          {|<div class="row-item tenant-row">
  <div style="display:flex;align-items:center;gap:6px">
    <span class="dot %s" title="%s"></span>
    <div class="color-swatch" style="background:%s"></div>
  </div>
  <div class="tenant-info">
    <div class="tenant-id">%s</div>
    <div class="tenant-label">%s%s</div>
  </div>
  <div class="tenant-cmd" title="%s">%s</div>
  <div class="row-actions">
    <button class="btn-icon" title="Edit" data-edit-tenant="%s">✏️</button>
    <button class="btn-icon" title="Delete" data-del-tenant="%s">🗑️</button>
  </div>
</div>|}
          dot_class dot_title
          (Page_util.escape_html t.color)
          (Page_util.escape_html id)
          (Page_util.escape_html t.label)
          brand_html cmd_title cmd_html
          (Page_util.escape_html id)
          (Page_util.escape_html id))
      |> String.concat
    in
    Page_util.set_html tenant_list_el html;
    Page_util.bind_clicks tenant_list_el
      ~selector:"[data-edit-tenant]" ~attr:"data-edit-tenant"
      ~f:edit_tenant;
    Page_util.bind_clicks tenant_list_el
      ~selector:"[data-del-tenant]" ~attr:"data-del-tenant"
      ~f:delete_tenant

and edit_tenant id =
  match List.Assoc.find !config.tenants ~equal:String.equal id with
  | None -> ()
  | Some t ->
    editing_tenant_id := Some id;
    let tf_id = Page_util.input_by_id "tfId" in
    tf_id##.value := Js.string id;
    tf_id##.disabled := Js._true;
    (Page_util.input_by_id "tfLabel")##.value := Js.string t.label;
    (Page_util.input_by_id "tfColor")##.value := Js.string t.color;
    let cmd_field = Page_util.input_by_id "tfCmd" in
    cmd_field##.value :=
      Js.string (Option.value t.browser_cmd ~default:"");
    cmd_field##.placeholder :=
      Js.string
        (match browser_cmd_from_brand t.brand with
         | "" -> "(optional)"
         | s -> s);
    Page_util.set_text (Page_util.get_by_id "tfSave") "Update tenant";
    Page_util.add_class tenant_form_el "visible"

and delete_tenant id =
  config :=
    { !config with
      tenants =
        List.Assoc.remove !config.tenants ~equal:String.equal id
    };
  render_tenants ();
  fetch_and_render_rules ();
  populate_tenant_selects ()

and save_tenant () =
  let id = Js.to_string (Page_util.input_by_id "tfId")##.value |> String.strip in
  let label = Js.to_string (Page_util.input_by_id "tfLabel")##.value |> String.strip in
  let color = Js.to_string (Page_util.input_by_id "tfColor")##.value in
  let cmd_raw = Js.to_string (Page_util.input_by_id "tfCmd")##.value |> String.strip in
  match String.is_empty id with
  | true -> show_msg "Tenant ID is required." "error"
  | false ->
    let existing =
      List.Assoc.find !config.tenants ~equal:String.equal id
    in
    let entry : Protocol.tenant_config =
      {
        label = (match String.is_empty label with true -> id | false -> label);
        color;
        browser_cmd =
          (match String.is_empty cmd_raw with true -> None | false -> Some cmd_raw);
        brand = Option.bind existing ~f:(fun e -> e.brand);
      }
    in
    config :=
      { !config with
        tenants =
          List.Assoc.add !config.tenants ~equal:String.equal id entry
      };
    reset_tenant_form ();
    render_tenants ();
    populate_tenant_selects ()

and reset_tenant_form () =
  editing_tenant_id := None;
  let tf_id = Page_util.input_by_id "tfId" in
  tf_id##.value := Js.string "";
  tf_id##.disabled := Js._false;
  (Page_util.input_by_id "tfLabel")##.value := Js.string "";
  (Page_util.input_by_id "tfColor")##.value := Js.string "#1a73e8";
  let cmd_field = Page_util.input_by_id "tfCmd" in
  cmd_field##.value := Js.string "";
  cmd_field##.placeholder := Js.string "(optional)";
  Page_util.set_text (Page_util.get_by_id "tfSave") "Add tenant";
  Page_util.remove_class tenant_form_el "visible"

(* -- Rule CRUD -- *)

and fetch_and_render_rules () =
  send_command Get_rules ()
    ~on_response:(fun result ->
      match result with
      | Ok r -> render_rules r
      | Error msg ->
        show_msg (Printf.sprintf "Failed to load rules: %s" msg) "error")

and send_and_refetch_rules updated =
  send_command Set_rules updated
    ~on_response:(fun result ->
      match result with
      | Ok () -> fetch_and_render_rules ()
      | Error msg ->
        show_msg (Printf.sprintf "Error saving rules: %s" msg) "error")

and render_rules rules =
  let resolve_label target =
    match List.Assoc.find !config.tenants ~equal:String.equal target with
    | Some tc ->
      (match String.is_empty tc.label with true -> target | false -> tc.label)
    | None -> target
  in
  match List.is_empty rules with
  | true ->
    Page_util.set_html rule_list_el
      {|<div class="card-empty">No routing rules configured.</div>|}
  | false ->
    let html =
      List.mapi rules ~f:(fun i r ->
        let on_class = match r.enabled with true -> "on" | false -> "" in
        Printf.sprintf
          {|<div class="row-item rule-row">
  <button class="toggle %s" data-toggle-rule="%d"></button>
  <div class="rule-pattern" title="%s">%s</div>
  <span class="rule-target">→ %s</span>
  <div class="row-actions">
    <button class="btn-icon" title="Edit" data-edit-rule="%d">✏️</button>
    <button class="btn-icon" title="Delete" data-del-rule="%d">🗑️</button>
    <button class="btn-icon" title="Move up" data-move-rule-up="%d">↑</button>
    <button class="btn-icon" title="Move down" data-move-rule-down="%d">↓</button>
  </div>
</div>|}
          on_class i
          (Page_util.escape_html r.pattern)
          (Page_util.escape_html r.pattern)
          (Page_util.escape_html (resolve_label r.target))
          i i i i)
      |> String.concat
    in
    Page_util.set_html rule_list_el html;
    Page_util.bind_clicks rule_list_el
      ~selector:"[data-toggle-rule]" ~attr:"data-toggle-rule"
      ~f:(fun idx_s ->
        let idx = Int.of_string idx_s in
        let updated =
          List.mapi rules ~f:(fun i r ->
            match Int.equal i idx with
            | true -> { r with enabled = not r.enabled }
            | false -> r)
        in
        send_and_refetch_rules updated);
    Page_util.bind_clicks rule_list_el
      ~selector:"[data-edit-rule]" ~attr:"data-edit-rule"
      ~f:(fun idx_s -> edit_rule rules (Int.of_string idx_s));
    Page_util.bind_clicks rule_list_el
      ~selector:"[data-del-rule]" ~attr:"data-del-rule"
      ~f:(fun idx_s ->
        let idx = Int.of_string idx_s in
        let updated =
          List.filteri rules ~f:(fun i _ -> not (Int.equal i idx))
        in
        send_and_refetch_rules updated);
    Page_util.bind_clicks rule_list_el
      ~selector:"[data-move-rule-up]" ~attr:"data-move-rule-up"
      ~f:(fun idx_s ->
        let idx = Int.of_string idx_s in
        (match idx > 0 with
         | false -> ()
         | true ->
           send_and_refetch_rules (swap_nth (idx - 1) rules)));
    Page_util.bind_clicks rule_list_el
      ~selector:"[data-move-rule-down]" ~attr:"data-move-rule-down"
      ~f:(fun idx_s ->
        let idx = Int.of_string idx_s in
        (match idx < List.length rules - 1 with
         | false -> ()
         | true ->
           send_and_refetch_rules (swap_nth idx rules)))

and edit_rule rules idx =
  match List.nth rules idx with
  | None -> ()
  | Some r ->
    editing_rule_index := Some idx;
    (Page_util.input_by_id "rfPattern")##.value := Js.string r.pattern;
    populate_rule_target r.target;
    Page_util.set_text (Page_util.get_by_id "rfSave") "Update rule";
    Page_util.add_class rule_form_el "visible"

and save_rule () =
  let pattern =
    Js.to_string (Page_util.input_by_id "rfPattern")##.value |> String.strip
  in
  let target = Js.to_string (Page_util.select_by_id "rfTarget")##.value in
  match String.is_empty pattern with
  | true -> show_msg "Pattern is required." "error"
  | false ->
    (match String.is_empty target with
     | true -> show_msg "Select a target tenant." "error"
     | false ->
       (match Page_util.validate_regexp pattern with
        | Error msg ->
          show_msg (Printf.sprintf "Invalid regex: %s" msg) "error"
        | Ok () ->
          let rule : Protocol.rule = { pattern; target; enabled = true } in
          send_command Get_rules ()
            ~on_response:(fun result ->
              match result with
              | Ok current ->
                let updated =
                  match !editing_rule_index with
                  | Some idx ->
                    let prev_enabled =
                      match List.nth current idx with
                      | Some r -> r.enabled
                      | None -> true
                    in
                    List.mapi current ~f:(fun i r ->
                      match Int.equal i idx with
                      | true -> { rule with enabled = prev_enabled }
                      | false -> r)
                  | None -> current @ [ rule ]
                in
                reset_rule_form ();
                send_and_refetch_rules updated
              | Error msg -> show_msg (Printf.sprintf "Failed to fetch rules: %s" msg) "error")))

and reset_rule_form () =
  editing_rule_index := None;
  (Page_util.input_by_id "rfPattern")##.value := Js.string "";
  Page_util.set_text (Page_util.get_by_id "rfSave") "Add rule";
  Page_util.remove_class rule_form_el "visible"

(* -- Defaults -- *)

let render_defaults () =
  let d = !config.defaults in
  (Page_util.input_by_id "dfCooldown")##.value :=
    Js.string (Int.to_string d.cooldown_seconds);
  (Page_util.input_by_id "dfTimeout")##.value :=
    Js.string (Int.to_string d.browser_launch_timeout);
  populate_tenant_selects ()

let read_defaults () =
  let cooldown =
    Js.to_string (Page_util.input_by_id "dfCooldown")##.value
    |> Int.of_string_opt
    |> Option.value ~default:5
  in
  let timeout =
    Js.to_string (Page_util.input_by_id "dfTimeout")##.value
    |> Int.of_string_opt
    |> Option.value ~default:10
  in
  let unmatched =
    Js.to_string (Page_util.select_by_id "dfUnmatched")##.value
  in
  config :=
    { !config with
      defaults = { unmatched; cooldown_seconds = cooldown; browser_launch_timeout = timeout }
    }

(* -- Save config -- *)

let save_config () =
  read_defaults ();
  show_msg "Saving\u{2026}" "";
  send_command Set_config !config
    ~on_response:(fun result ->
      match result with
      | Ok () -> show_msg "Configuration saved." "success"
      | Error msg -> show_msg (Printf.sprintf "Error: %s" msg) "error")

(* -- Fetch config + status -- *)

let fetch_config () =
  let pending = ref 3 in
  let config_ok = ref false in
  let rules_ok = ref false in
  let fetched_rules = ref [] in

  let finish_init () =
    match !pending > 0 with
    | true -> ()
    | false ->
      (match !config_ok && !rules_ok with
       | false ->
         set_status false;
         Page_util.set_text loading_el "Failed to load configuration."
       | true ->
         Page_util.set_display loading_el "none";
         Page_util.set_display content_el "block";
         render_tenants ();
         render_rules !fetched_rules;
         render_defaults ())
  in

  send_command Get_config ()
    ~on_response:(fun result ->
      (match result with
       | Ok cfg ->
         set_status true;
         config := cfg;
         config_ok := true
       | Error _ -> set_status false);
      pending := !pending - 1;
      finish_init ());

  send_command Get_rules ()
    ~on_response:(fun result ->
      (match result with
       | Ok r ->
         fetched_rules := r;
         rules_ok := true
       | Error _ -> ());
      pending := !pending - 1;
      finish_init ());

  send_command Status ()
    ~on_response:(fun result ->
      (match result with
       | Ok info ->
         connected_tenants :=
           Set.of_list (module String) info.registered_tenants
       | Error _ -> ());
      pending := !pending - 1;
      finish_init ())

(* -- Event bindings -- *)

let () =
  Page_util.on_click (Page_util.get_by_id "btnAddTenant") (fun () ->
    reset_tenant_form ();
    Page_util.add_class tenant_form_el "visible");
  Page_util.on_click (Page_util.get_by_id "tfCancel") (fun () ->
    reset_tenant_form ());
  Page_util.on_click (Page_util.get_by_id "tfSave") (fun () ->
    save_tenant ());
  Page_util.on_click (Page_util.get_by_id "btnAddRule") (fun () ->
    reset_rule_form ();
    populate_rule_target "";
    Page_util.add_class rule_form_el "visible");
  Page_util.on_click (Page_util.get_by_id "rfCancel") (fun () ->
    reset_rule_form ());
  Page_util.on_click (Page_util.get_by_id "rfSave") (fun () ->
    save_rule ());
  Page_util.on_click (Page_util.get_by_id "btnSave") (fun () ->
    save_config ())

(* -- Init -- *)

let () =
  Page_util.connect_port
    ~on_ready:(fun conn ->
      conn_ref := Some conn;
      fetch_config ())
    ~on_disconnect:(fun () ->
      conn_ref := None;
      set_status false)
    ~on_event:(fun _p -> ())
