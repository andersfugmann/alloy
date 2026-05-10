open! Base
open! Stdio
open Js_of_ocaml

let ( let* ) = Lwt.bind

let search_input = Page_util.input_by_id "searchInput"
let results_list = Page_util.get_by_id "resultsList"
let status_el = Page_util.get_by_id "status"
let search_urls_cb = Page_util.input_by_id "searchUrls"
let search_titles_cb = Page_util.input_by_id "searchTitles"
let sort_select = Page_util.select_by_id "sortBy"
let max_age_select = Page_util.select_by_id "maxAge"

let search_gen = ref 0
let cached_results : Protocol.lookup_result list ref = ref []
let context_menu = Page_util.get_by_id "contextMenu"
let ctx_remove = Page_util.get_by_id "ctxRemove"
let pending_delete_urls : string list ref = ref []
let active_conn : Client.t option ref = ref None

let hide_context_menu () =
  Page_util.set_display context_menu "none";
  pending_delete_urls := []

let show_context_menu ~x ~y ~urls =
  pending_delete_urls := urls;
  context_menu##.style##.left := Js.string (Printf.sprintf "%dpx" x);
  context_menu##.style##.top := Js.string (Printf.sprintf "%dpx" y);
  Page_util.set_display context_menu "block"

let sort_results sort_mode results =
  match sort_mode with
  | "date" ->
    List.sort results ~compare:(fun (a : Protocol.lookup_result) b ->
      let recent_a = List.hd a.entry.visits |> Option.value ~default:0 in
      let recent_b = List.hd b.entry.visits |> Option.value ~default:0 in
      Int.compare recent_b recent_a)
  | "frequency" ->
    List.sort results ~compare:(fun (a : Protocol.lookup_result) b ->
      Int.compare (List.length b.entry.visits) (List.length a.entry.visits))
  | _ -> results

let today () =
  let ms = Js.float_of_number (new%js Js.date_now)##getTime in
  Float.to_int (ms /. 86400000.)

let days_ago_text days =
  match days with
  | 0 -> "today"
  | 1 -> "yesterday"
  | n -> Printf.sprintf "%d days ago" n

let path_of_url url =
  match Page_util.url_origin url with
  | Some origin ->
    let path = String.drop_prefix url (String.length origin) in
    begin match String.is_empty path with
    | true -> "/"
    | false -> path
    end
  | None -> url

let display_domain url =
  match Page_util.url_origin url with
  | Some origin ->
    begin match String.substr_index origin ~pattern:"://" with
    | Some i -> String.drop_prefix origin (i + 3)
    | None -> origin
    end
  | None -> url

let group_key (r : Protocol.lookup_result) =
  match String.is_empty r.entry.title with
  | true -> None
  | false ->
    let domain = Page_util.url_origin r.entry.url |> Option.value ~default:"" in
    Some (String.lowercase r.entry.title, String.lowercase domain)

let equal_key a b =
  match (a, b) with
  | (Some (t1, d1), Some (t2, d2)) -> String.equal t1 t2 && String.equal d1 d2
  | _ -> false

let group_results results =
  let rec insert k r = function
    | [] -> [(k, [r])]
    | (gk, rs) :: rest ->
      begin match equal_key gk k with
      | true -> (gk, rs @ [r]) :: rest
      | false -> (gk, rs) :: insert k r rest
      end
  in
  List.fold results ~init:[] ~f:(fun groups r -> insert (group_key r) r groups)

let visit_meta_text visits =
  let last_visit =
    match List.hd visits with
    | Some day -> days_ago_text (today () - day)
    | None -> "no visits"
  in
  let count = List.length visits in
  Printf.sprintf "%s · %d visit%s" last_visit count
    (match count with 1 -> "" | _ -> "s")

let render_results results =
  Page_util.set_html results_list "";
  hide_context_menu ();
  let doc = Dom_html.document in
  let sort_mode = Js.to_string sort_select##.value in
  let sorted = sort_results sort_mode results in
  match sorted with
  | [] ->
    Page_util.set_html results_list
      {|<li class="empty-state">No results</li>|}
  | _ ->
    let groups = group_results sorted in
    let total_results = List.length sorted in
    let on_context_menu (el : Dom_html.element Js.t) urls =
      ignore (Dom_html.addEventListener el
        (Dom_html.Event.make "contextmenu")
        (Dom_html.handler (fun ev ->
          Dom.preventDefault ev;
          show_context_menu
            ~x:ev##.clientX
            ~y:ev##.clientY
            ~urls;
          Js._false))
        Js._false)
    in
    let render_single (entry : Protocol.history_entry) =
      let li = Dom_html.createLi doc in
      let title_div = Dom_html.createDiv doc in
      Page_util.set_class (title_div :> Dom_html.element Js.t) "result-title";
      let display_title =
        match String.is_empty entry.title with
        | true -> entry.url
        | false -> entry.title
      in
      Page_util.set_text (title_div :> Dom_html.element Js.t) display_title;
      Dom.appendChild li title_div;
      let url_div = Dom_html.createDiv doc in
      Page_util.set_class (url_div :> Dom_html.element Js.t) "result-url";
      Page_util.set_text (url_div :> Dom_html.element Js.t) entry.url;
      Dom.appendChild li url_div;
      let meta_div = Dom_html.createDiv doc in
      Page_util.set_class (meta_div :> Dom_html.element Js.t) "result-meta";
      Page_util.set_text (meta_div :> Dom_html.element Js.t)
        (visit_meta_text entry.visits);
      Dom.appendChild li meta_div;
      Page_util.on_click (li :> Dom_html.element Js.t) (fun () ->
        Chrome_api.Tabs.create_url entry.url);
      on_context_menu (li :> Dom_html.element Js.t) [entry.url];
      li
    in
    let render_group results =
      let first : Protocol.lookup_result = List.hd_exn results in
      let li = Dom_html.createLi doc in
      Page_util.set_class (li :> Dom_html.element Js.t) "result-group";
      let header = Dom_html.createDiv doc in
      Page_util.set_class (header :> Dom_html.element Js.t) "group-header";
      let toggle = Dom_html.createSpan doc in
      Page_util.set_class (toggle :> Dom_html.element Js.t) "group-toggle";
      Page_util.set_text (toggle :> Dom_html.element Js.t) "▸";
      Dom.appendChild header toggle;
      let info = Dom_html.createDiv doc in
      Page_util.set_class (info :> Dom_html.element Js.t) "group-info";
      let title_div = Dom_html.createDiv doc in
      Page_util.set_class (title_div :> Dom_html.element Js.t) "result-title";
      Page_util.set_text (title_div :> Dom_html.element Js.t) first.entry.title;
      Dom.appendChild info title_div;
      let url_div = Dom_html.createDiv doc in
      Page_util.set_class (url_div :> Dom_html.element Js.t) "result-url";
      let domain = display_domain first.entry.url in
      let count = List.length results in
      Page_util.set_text (url_div :> Dom_html.element Js.t)
        (Printf.sprintf "%s · %d page%s" domain count
          (match count with 1 -> "" | _ -> "s"));
      Dom.appendChild info url_div;
      let meta_div = Dom_html.createDiv doc in
      Page_util.set_class (meta_div :> Dom_html.element Js.t) "result-meta";
      let most_recent =
        List.filter_map results ~f:(fun r -> List.hd r.entry.visits)
        |> List.max_elt ~compare:Int.compare
      in
      let total_visits =
        List.sum (module Int) results ~f:(fun r -> List.length r.entry.visits)
      in
      let last_text =
        match most_recent with
        | Some day -> days_ago_text (today () - day)
        | None -> "no visits"
      in
      Page_util.set_text (meta_div :> Dom_html.element Js.t)
        (Printf.sprintf "%s · %d visit%s total" last_text total_visits
          (match total_visits with 1 -> "" | _ -> "s"));
      Dom.appendChild info meta_div;
      Dom.appendChild header info;
      Dom.appendChild li header;
      let children_ul = Dom_html.createUl doc in
      Page_util.set_class (children_ul :> Dom_html.element Js.t) "group-children";
      Page_util.set_display (children_ul :> Dom_html.element Js.t) "none";
      List.iter results ~f:(fun (r : Protocol.lookup_result) ->
        let child_li = Dom_html.createLi doc in
        let path_div = Dom_html.createDiv doc in
        Page_util.set_class (path_div :> Dom_html.element Js.t) "result-url";
        Page_util.set_text (path_div :> Dom_html.element Js.t)
          (path_of_url r.entry.url);
        Dom.appendChild child_li path_div;
        let child_meta = Dom_html.createDiv doc in
        Page_util.set_class (child_meta :> Dom_html.element Js.t) "result-meta";
        Page_util.set_text (child_meta :> Dom_html.element Js.t)
          (visit_meta_text r.entry.visits);
        Dom.appendChild child_li child_meta;
        Page_util.on_click (child_li :> Dom_html.element Js.t) (fun () ->
          Chrome_api.Tabs.create_url r.entry.url);
        Dom.appendChild children_ul child_li);
      Dom.appendChild li children_ul;
      let expanded = ref false in
      Page_util.on_click (header :> Dom_html.element Js.t) (fun () ->
        expanded := not !expanded;
        match !expanded with
        | true ->
          Page_util.set_display (children_ul :> Dom_html.element Js.t) "block";
          Page_util.set_text (toggle :> Dom_html.element Js.t) "▾"
        | false ->
          Page_util.set_display (children_ul :> Dom_html.element Js.t) "none";
          Page_util.set_text (toggle :> Dom_html.element Js.t) "▸");
      let all_urls = List.map results ~f:(fun (r : Protocol.lookup_result) -> r.entry.url) in
      on_context_menu (li :> Dom_html.element Js.t) all_urls;
      li
    in
    List.iter groups ~f:(fun (_, group) ->
      let li =
        match group with
        | [r] -> render_single r.entry
        | rs -> render_group rs
      in
      Dom.appendChild results_list li);
    let group_count = List.length groups in
    let status_text =
      match Int.equal total_results group_count with
      | true ->
        Printf.sprintf "%d result%s" total_results
          (match total_results with 1 -> "" | _ -> "s")
      | false ->
        Printf.sprintf "%d result%s · %d group%s" total_results
          (match total_results with 1 -> "" | _ -> "s")
          group_count
          (match group_count with 1 -> "" | _ -> "s")
    in
    Page_util.set_text status_el status_text

let get_max_age_days () =
  let v = Js.to_string max_age_select##.value in
  match String.is_empty v with
  | true -> None
  | false -> Int.of_string_opt v

let get_scope () : Protocol.search_scope =
  let urls = Js.to_bool search_urls_cb##.checked in
  let titles = Js.to_bool search_titles_cb##.checked in
  match (urls, titles) with
  | (true, true) -> Both
  | (true, false) -> Url
  | (false, true) -> Title
  | (false, false) -> Both

let do_search conn =
  let query = Js.to_string search_input##.value in
  match String.is_empty query with
  | true ->
    cached_results := [];
    Page_util.set_html results_list "";
    Page_util.set_text status_el ""
  | false ->
    Lwt.async (fun () ->
      let scope = get_scope () in
      let max_age_days = get_max_age_days () in
      let* result = Client.call conn Lookup { query; scope; max_results = 100; max_age_days } in
      begin match result with
      | Ok results ->
        cached_results := results;
        render_results results
      | Error msg ->
        cached_results := [];
        Page_util.set_html results_list "";
        Page_util.set_text status_el (Printf.sprintf "Error: %s" msg)
      end;
      Lwt.return_unit)

let schedule_search conn =
  let gen = !search_gen + 1 in
  search_gen := gen;
  Chrome_api.set_timeout
    (fun () ->
      match Int.equal !search_gen gen with
      | true -> do_search conn
      | false -> ())
    300

let delete_urls conn urls =
  let url_set = Set.of_list (module String) urls in
  Lwt.async (fun () ->
    let* _result = Client.call conn Delete_history urls in
    cached_results :=
      List.filter !cached_results ~f:(fun (r : Protocol.lookup_result) ->
        not (Set.mem url_set r.entry.url));
    render_results !cached_results;
    Lwt.return_unit)

let () =
  Chrome_api.log "History panel starting";
  Dom_html.document##.onclick :=
    Dom_html.handler (fun _ev -> hide_context_menu (); Js._true);
  ignore (Dom_html.addEventListener Dom_html.document
    (Dom_html.Event.make "keydown")
    (Dom_html.handler (fun (ev : Dom_html.keyboardEvent Js.t) ->
      let key = Js.Optdef.case ev##.key (fun () -> "") Js.to_string in
      match key with
      | "Escape" -> hide_context_menu (); Js._true
      | _ -> Js._true))
    Js._false);
  Page_util.connect_port
    ~on_ready:(fun conn ->
      active_conn := Some conn;
      Page_util.on_click ctx_remove (fun () ->
        let urls = !pending_delete_urls in
        hide_context_menu ();
        delete_urls conn urls);
      search_input##.oninput :=
        Dom_html.handler (fun _ev ->
          schedule_search conn;
          Js._true);
      (search_urls_cb :> Dom_html.element Js.t)##.onclick :=
        Dom_html.handler (fun _ev ->
          schedule_search conn;
          Js._true);
      (search_titles_cb :> Dom_html.element Js.t)##.onclick :=
        Dom_html.handler (fun _ev ->
          schedule_search conn;
          Js._true);
      ignore (Dom_html.addEventListener
        (sort_select :> Dom_html.element Js.t)
        Dom_html.Event.change
        (Dom_html.handler (fun _ev ->
          render_results !cached_results;
          Js._true))
        Js._false);
      ignore (Dom_html.addEventListener
        (max_age_select :> Dom_html.element Js.t)
        Dom_html.Event.change
        (Dom_html.handler (fun _ev ->
          do_search conn;
          Js._true))
        Js._false))
    ~on_disconnect:(fun () ->
      cached_results := [];
      Page_util.set_html results_list "";
      Page_util.set_text status_el "Disconnected")
    ~on_event:(fun _p -> ())
