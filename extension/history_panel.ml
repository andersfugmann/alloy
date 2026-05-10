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

let render_results results =
  Page_util.set_html results_list "";
  let doc = Dom_html.document in
  let sort_mode = Js.to_string sort_select##.value in
  let sorted = sort_results sort_mode results in
  match sorted with
  | [] ->
    Page_util.set_html results_list
      {|<li class="empty-state">No results</li>|}
  | _ ->
    List.iter sorted ~f:(fun (r : Protocol.lookup_result) ->
      let entry = r.entry in
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
      let last_visit =
        match List.hd entry.visits with
        | Some day -> days_ago_text (today () - day)
        | None -> "no visits"
      in
      let visit_count = List.length entry.visits in
      Page_util.set_text (meta_div :> Dom_html.element Js.t)
        (Printf.sprintf "%s · %d visit%s" last_visit visit_count
          (match visit_count with 1 -> "" | _ -> "s"));
      Dom.appendChild li meta_div;
      Page_util.on_click (li :> Dom_html.element Js.t) (fun () ->
        Chrome_api.Tabs.create_url entry.url);
      Dom.appendChild results_list li);
  let n = List.length sorted in
  Page_util.set_text status_el
    (Printf.sprintf "%d result%s" n
      (match n with 1 -> "" | _ -> "s"))

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

let () =
  Chrome_api.log "History panel starting";
  Page_util.connect_port
    ~on_ready:(fun conn ->
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
