(* chrome_api.ml — Typed OCaml bindings for Chrome extension APIs.
   All Js.Unsafe usage in the project is confined to this module. *)

open! Base
open! Stdio
open Js_of_ocaml

(* ── Internal unsafe primitives ──────────────────────────────────── *)

let global : _ Js.t = Js.Unsafe.global
let chrome : _ Js.t = Js.Unsafe.get global (Js.string "chrome")

let get (o : _ Js.t) k : _ Js.t =
  Js.Unsafe.get o (Js.string k)

let get_opt (o : _ Js.t) k =
  let v : _ Js.Optdef.t = Js.Unsafe.get o (Js.string k) in
  Js.Optdef.to_option v

let call (o : _ Js.t) (m : string) (args : Js.Unsafe.any array) : 'a =
  Js.Unsafe.meth_call o m args

let inject = Js.Unsafe.inject

let js_obj pairs : _ Js.t =
  Js.Unsafe.obj (Array.of_list pairs)

let add_listener (target : _ Js.t) event_name cb =
  call (get target event_name) "addListener" [| cb |]

(* ── JSON interop ────────────────────────────────────────────────── *)

let json_parse s : _ Js.t =
  Js._JSON##parse (Js.string s)

let json_stringify (v : _ Js.t) =
  Js.to_string (Js._JSON##stringify v)

(* ── Console ─────────────────────────────────────────────────────── *)

let log msg =
  Console.console##log (Js.string (Printf.sprintf "[alloy] %s" msg))

(* ── performance.now() ───────────────────────────────────────────── *)

class type performance = object
  method now : Js.number Js.t Js.meth
end

let performance_now () =
  let perf : performance Js.t = Js.Unsafe.coerce (get global "performance") in
  Js.float_of_number perf##now

(* ── setTimeout ──────────────────────────────────────────────────── *)

let set_timeout f ms =
  ignore
    (Js.Unsafe.meth_call global "setTimeout"
       [| inject (Js.wrap_callback f); inject ms |] : unit)

(* ── Opaque port type ────────────────────────────────────────────── *)

(* TODO: Move this into the port module *)
type port = < > Js.t

(* ── Shared runtime handle ────────────────────────────────────────── *)

let rt : _ Js.t = get chrome "runtime"

(* ── Port operations ─────────────────────────────────────────────── *)

module Port = struct
  let post_message (p : port) json =
    call p "postMessage" [| inject (Yojson.Safe.to_string json |> json_parse) |]

  let on_message (p : port) f =
    add_listener p "onMessage"
      (inject (Js.wrap_callback (fun msg -> json_stringify msg |> Yojson.Safe.from_string |> f)))

  let on_disconnect (p : port) f =
    add_listener p "onDisconnect"
      (inject (Js.wrap_callback (fun _port -> f ())))

  let disconnect (p : port) =
    call p "disconnect" [||]

  let connect () : port =
    let p : _ Js.t = call rt "connect" [||] in
    Js.Unsafe.coerce p

  let on_connect f =
    add_listener rt "onConnect"
      (inject (Js.wrap_callback (fun (p : _ Js.t) ->
        f (Js.Unsafe.coerce p : port))))

  module Native = struct
    let connect app : port =
      let p : _ Js.t = call rt "connectNative" [| inject (Js.string app) |] in
      Js.Unsafe.coerce p
  end
end

(* ── Runtime ─────────────────────────────────────────────────────── *)

module Runtime = struct
  let get_url path =
    Js.to_string
      (call rt "getURL" [| inject (Js.string path) |] : Js.js_string Js.t)

  let on_installed f =
    add_listener rt "onInstalled"
      (inject (Js.wrap_callback (fun _details -> f ())))

  let on_startup f =
    add_listener rt "onStartup"
      (inject (Js.wrap_callback (fun _unit -> f ())))
end

(* ── Tabs ────────────────────────────────────────────────────────── *)

module Tabs = struct
  let tabs : _ Js.t = get chrome "tabs"

  let create_url url =
    call tabs "create" [| inject (js_obj [ ("url", inject (Js.string url)) ]) |]

  let remove tab_id =
    call tabs "remove" [| inject tab_id |]

  let query_active ~on_result =
    let query = js_obj [ ("active", inject Js._true); ("currentWindow", inject Js._true) ] in
    let cb = Js.wrap_callback (fun (tabs_arr : _ Js.t) ->
      let len : int = Js.Unsafe.get tabs_arr (Js.string "length") in
      match len > 0 with
      | true ->
        let tab : _ Js.t = Js.Unsafe.get tabs_arr 0 in
        let url : Js.js_string Js.t = Js.Unsafe.get tab (Js.string "url") in
        let tab_id : int = Js.Unsafe.get tab (Js.string "id") in
        on_result (Js.to_string url) tab_id
      | false -> ())
    in
    call tabs "query" [| inject query; inject cb |]

  let get_title tab_id ~on_result =
    let cb = Js.wrap_callback (fun (tab : _ Js.t) ->
      let title : Js.js_string Js.t Js.Optdef.t = Js.Unsafe.get tab (Js.string "title") in
      on_result (Js.Optdef.case title (fun () -> None) (fun s -> Some (Js.to_string s))))
    in
    call tabs "get" [| inject tab_id; inject cb |]
end

(* ── Action (toolbar icon) ────────────────────────────────────────── *)

module Action = struct
  let action : _ Js.t = get chrome "action"

  let set_icon path_16 path_48 path_128 =
    let paths = js_obj [
      ("16", inject (Js.string path_16));
      ("48", inject (Js.string path_48));
      ("128", inject (Js.string path_128));
    ] in
    call action "setIcon" [| inject (js_obj [ ("path", inject paths) ]) |]
end

(* ── Windows ─────────────────────────────────────────────────────── *)

module Windows = struct
  let windows : _ Js.t = get chrome "windows"

  let create_popup ~url ~width ~height =
    call windows "create"
      [| inject
           (js_obj
              [
                ("url", inject (Js.string url));
                ("type", inject (Js.string "popup"));
                ("width", inject width);
                ("height", inject height);
              ]) |]

  let get_last_focused ~on_result =
    let cb = Js.wrap_callback (fun (win : _ Js.t) ->
      let id : int = Js.Unsafe.get win (Js.string "id") in
      on_result id)
    in
    call windows "getLastFocused" [| inject cb |]
end

(* ── Storage ─────────────────────────────────────────────────────── *)

module Storage = struct
  let local () : _ Js.t = get (get chrome "storage") "local"

  let get_local keys ~on_result =
    let keys_arr =
      keys |> List.map ~f:Js.string |> Array.of_list |> Js.array
    in
    call (local ()) "get"
      [| inject keys_arr;
         inject
           (Js.wrap_callback (fun items ->
              let pairs =
                List.filter_map keys ~f:(fun k ->
                    match get_opt items k with
                    | None -> None
                    | Some v ->
                      Some (k, Js.to_string (Js.Unsafe.coerce v)))
              in
              on_result pairs)) |]

  let set_local items ~on_done =
    let obj =
      js_obj
        (List.map items ~f:(fun (k, v) -> (k, inject (Js.string v))))
    in
    call (local ()) "set" [| inject obj; inject (Js.wrap_callback on_done) |]
end

(* ── Context Menus ───────────────────────────────────────────────── *)

module Context_menus = struct
  let menus () : _ Js.t = get chrome "contextMenus"

  let web_url_patterns =
    [| "http://*/*"; "https://*/*" |]
    |> Array.map ~f:Js.string |> Js.array

  let create ~id ~title ~contexts =
    let contexts_arr =
      contexts |> List.map ~f:Js.string |> Array.of_list |> Js.array
    in
    call (menus ()) "create"
      [| inject
           (js_obj
              [
                ("id", inject (Js.string id));
                ("title", inject (Js.string title));
                ("contexts", inject contexts_arr);
                ("documentUrlPatterns", inject web_url_patterns);
              ]) |]

  let create_child ~id ~parent_id ~title ~contexts ?(enabled = true) () =
    let contexts_arr =
      contexts |> List.map ~f:Js.string |> Array.of_list |> Js.array
    in
    call (menus ()) "create"
      [| inject
           (js_obj
              [
                ("id", inject (Js.string id));
                ("parentId", inject (Js.string parent_id));
                ("title", inject (Js.string title));
                ("contexts", inject contexts_arr);
                ("documentUrlPatterns", inject web_url_patterns);
                ("enabled", inject (Js.bool enabled));
              ]) |]

  let remove_all f =
    call (menus ()) "removeAll" [| inject (Js.wrap_callback f) |]

  class type click_info = object
    method menuItemId : Js.js_string Js.t Js.readonly_prop
    method linkUrl : Js.js_string Js.t Js.Optdef.t Js.readonly_prop
    method pageUrl : Js.js_string Js.t Js.Optdef.t Js.readonly_prop
  end

  class type tab_info = object
    method id : int Js.Optdef.t Js.readonly_prop
  end

  let on_clicked f =
    add_listener (menus ()) "onClicked"
      (inject
         (Js.wrap_callback (fun (info : click_info Js.t) (tab : tab_info Js.t) ->
              let menu_id = Js.to_string info##.menuItemId in
              let link_url =
                Js.Optdef.case info##.linkUrl
                  (fun () -> "")
                  Js.to_string
              in
              let page_url =
                Js.Optdef.case info##.pageUrl
                  (fun () -> "")
                  Js.to_string
              in
              let tab_id = Js.Optdef.to_option tab##.id in
              f menu_id link_url page_url tab_id)))
end

(* ── Web Navigation ──────────────────────────────────────────────── *)

module Web_navigation = struct
  let nav () : _ Js.t = get chrome "webNavigation"

  class type nav_details = object
    method url : Js.js_string Js.t Js.readonly_prop
    method tabId : int Js.readonly_prop
    method frameId : int Js.readonly_prop
  end

  class type commit_details = object
    method url : Js.js_string Js.t Js.readonly_prop
    method tabId : int Js.readonly_prop
    method frameId : int Js.readonly_prop
    method transitionQualifiers :
      Js.js_string Js.t Js.js_array Js.t Js.Optdef.t Js.readonly_prop
  end

  let on_before_navigate f =
    add_listener (nav ()) "onBeforeNavigate"
      (inject
         (Js.wrap_callback (fun (details : nav_details Js.t) ->
              f (Js.to_string details##.url) details##.tabId details##.frameId)))

  let on_completed f =
    add_listener (nav ()) "onCompleted"
      (inject
         (Js.wrap_callback (fun (details : nav_details Js.t) ->
              f (Js.to_string details##.url) details##.tabId details##.frameId)))

  let on_committed f =
    add_listener (nav ()) "onCommitted"
      (inject
         (Js.wrap_callback (fun (details : commit_details Js.t) ->
              let qualifiers =
                Js.Optdef.case details##.transitionQualifiers
                  (fun () -> [])
                  (fun arr ->
                     let len = arr##.length in
                     List.init len ~f:(fun i ->
                       Js.Optdef.case (Js.array_get arr i)
                         (fun () -> "")
                         Js.to_string))
              in
              f (Js.to_string details##.url) details##.tabId
                details##.frameId ~transition_qualifiers:qualifiers)))
end

(* ── Web Request ─────────────────────────────────────────────────── *)

module Web_request = struct
  let req () : _ Js.t = get chrome "webRequest"

  class type request_details = object
    method url : Js.js_string Js.t Js.readonly_prop
    method tabId : int Js.readonly_prop
    method statusCode : int Js.readonly_prop
  end

  let on_completed f =
    let filter = js_obj [ ("urls", inject (Js.array [| Js.string "http://*/*"; Js.string "https://*/*" |]));
                          ("types", inject (Js.array [| Js.string "main_frame" |])) ] in
    call (get (req ()) "onCompleted") "addListener"
      [| inject (Js.wrap_callback (fun (details : request_details Js.t) ->
             f (Js.to_string details##.url) details##.tabId details##.statusCode));
         inject filter |]
end

(* ── Navigator (browser brand detection) ─────────────────────────── *)

module Navigator = struct
  let is_grease_brand b =
    String.is_prefix b ~prefix:"Not"

  class type brand_entry = object
    method brand : Js.js_string Js.t Js.readonly_prop
  end

  class type user_agent_data = object
    method brands : brand_entry Js.t Js.js_array Js.t Js.Optdef.t Js.readonly_prop
  end

  class type navigator = object
    method userAgentData : user_agent_data Js.t Js.Optdef.t Js.readonly_prop
    method userAgent : Js.js_string Js.t Js.readonly_prop
  end

  let brand_from_user_agent ua =
    let ua_lower = String.lowercase ua in
    let candidates =
      [ ("edg/", "Microsoft Edge");
        ("opr/", "Opera");
        ("brave/", "Brave");
        ("vivaldi/", "Vivaldi");
        ("chrome/", "Google Chrome");
        ("chromium/", "Chromium") ]
    in
    List.find_map candidates ~f:(fun (token, name) ->
        match String.is_substring ua_lower ~substring:token with
        | true -> Some name
        | false -> None)
    |> Option.value ~default:""

  let brand_from_ua_data (uad : user_agent_data Js.t) =
    Js.Optdef.case uad##.brands
      (fun () -> "")
      (fun arr ->
         let len = arr##.length in
         let brands =
           List.init len ~f:(fun i ->
               Js.Optdef.case (Js.array_get arr i)
                 (fun () -> "")
                 (fun entry -> Js.to_string entry##.brand))
         in
         let non_grease =
           List.filter brands ~f:(fun b ->
               not (is_grease_brand b) && not (String.is_empty b))
         in
         match
           List.find non_grease ~f:(fun b ->
               not (String.equal b "Chromium"))
         with
         | Some b -> b
         | None ->
           (match non_grease with
            | b :: _ -> b
            | [] -> ""))

  let get_browser_brand () =
    match get_opt global "navigator" with
    | None -> ""
    | Some nav_js ->
      let nav : navigator Js.t = Js.Unsafe.coerce nav_js in
      let from_ua_data =
        Js.Optdef.case nav##.userAgentData
          (fun () -> "")
          brand_from_ua_data
      in
      match String.is_empty from_ua_data with
      | false -> from_ua_data
      | true -> brand_from_user_agent (Js.to_string nav##.userAgent)
end

(* ── Commands ────────────────────────────────────────────────────── *)

module Commands = struct
  let on_command f =
    add_listener (get chrome "commands") "onCommand"
      (inject (Js.wrap_callback (fun (cmd : Js.js_string Js.t) ->
        f (Js.to_string cmd))))
end

(* ── Side Panel ──────────────────────────────────────────────────── *)

module Side_panel = struct
  let open_panel ~window_id =
    let opts = js_obj [("windowId", inject window_id)] in
    call (get chrome "sidePanel") "open" [| inject opts |]
end

(* ── History ─────────────────────────────────────────────────────── *)

module History = struct
  let history () : _ Js.t = get chrome "history"

  class type history_item = object
    method url : Js.js_string Js.t Js.Optdef.t Js.readonly_prop
    method title : Js.js_string Js.t Js.Optdef.t Js.readonly_prop
    method lastVisitTime : Js.number Js.t Js.Optdef.t Js.readonly_prop
  end

  class type visit_item = object
    method visitTime : Js.number Js.t Js.Optdef.t Js.readonly_prop
  end

  let search ~max_results ~f ~on_done =
    let query = js_obj [
      ("text", inject (Js.string ""));
      ("maxResults", inject max_results);
      ("startTime", inject 0);
    ] in
    let cb = Js.wrap_callback (fun (items : history_item Js.t Js.js_array Js.t) ->
      let len = items##.length in
      List.init len ~f:(fun i ->
        Js.Optdef.case (Js.array_get items i)
          (fun () -> ())
          (fun item ->
            let url = Js.Optdef.case item##.url (fun () -> "") Js.to_string in
            let title = Js.Optdef.case item##.title (fun () -> "") Js.to_string in
            let last_visit_time =
              Js.Optdef.case item##.lastVisitTime (fun () -> 0.0) Js.float_of_number
            in
            f ~url ~title ~last_visit_time))
      |> (ignore : unit list -> unit);
      on_done ())
    in
    call (history ()) "search" [| inject query; inject cb |]

  let get_visits url ~f =
    let query = js_obj [ ("url", inject (Js.string url)) ] in
    let cb = Js.wrap_callback (fun (items : visit_item Js.t Js.js_array Js.t) ->
      let len = items##.length in
      let times = List.init len ~f:(fun i ->
        Js.Optdef.case (Js.array_get items i)
          (fun () -> 0.0)
          (fun item ->
            Js.Optdef.case item##.visitTime (fun () -> 0.0) Js.float_of_number))
      in
      f times)
    in
    call (history ()) "getVisits" [| inject query; inject cb |]
end
