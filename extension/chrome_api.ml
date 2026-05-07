(* chrome_api.ml — Typed OCaml bindings for Chrome extension APIs.
   All Js.Unsafe usage in the project is confined to this module. *)

open! Base
open! Stdio
open Js_of_ocaml

(* ── Internal unsafe primitives ──────────────────────────────────── *)

let global : _ Js.t = Js.Unsafe.global
let chrome : _ Js.t = Js.Unsafe.get global (Js.string "chrome")

let get (o : _ Js.t) (k : string) : _ Js.t =
  Js.Unsafe.get o (Js.string k)

let get_opt (o : _ Js.t) (k : string) : _ Js.t option =
  let v : _ Js.Optdef.t = Js.Unsafe.get o (Js.string k) in
  Js.Optdef.to_option v

let call (o : _ Js.t) (m : string) (args : Js.Unsafe.any array) : 'a =
  Js.Unsafe.meth_call o m args

let inject = Js.Unsafe.inject

let js_obj (pairs : (string * Js.Unsafe.any) list) : _ Js.t =
  Js.Unsafe.obj (Array.of_list pairs)

let add_listener (target : _ Js.t) (event_name : string)
    (cb : Js.Unsafe.any) : unit =
  call (get target event_name) "addListener" [| cb |]

(* ── JSON interop ────────────────────────────────────────────────── *)

let json_parse (s : string) : _ Js.t =
  Js._JSON##parse (Js.string s)

let json_stringify (v : _ Js.t) : string =
  Js.to_string (Js._JSON##stringify v)

let safe_stringify (v : _ Js.t) : string =
  match Js.to_string (Js._JSON##stringify v) with
  | s -> s
  | exception _ -> "null"

(* ── Console ─────────────────────────────────────────────────────── *)

let log (msg : string) : unit =
  Console.console##log (Js.string (Printf.sprintf "[alloy] %s" msg))

(* ── performance.now() ───────────────────────────────────────────── *)

class type performance = object
  method now : Js.number Js.t Js.meth
end

let performance_now () : float =
  let perf : performance Js.t = Js.Unsafe.coerce (get global "performance") in
  Js.float_of_number perf##now

(* ── setTimeout ──────────────────────────────────────────────────── *)

let set_timeout (f : unit -> unit) (ms : int) : unit =
  ignore
    (Js.Unsafe.meth_call global "setTimeout"
       [| inject (Js.wrap_callback f); inject ms |] : unit)

(* ── Opaque port type ────────────────────────────────────────────── *)

type port = < > Js.t

(* ── Port operations ─────────────────────────────────────────────── *)

module Port = struct
  let post_message_json (p : port) (json_str : string) : unit =
    call p "postMessage" [| inject (json_parse json_str) |]

  let on_message_json (p : port) (f : string -> unit) : unit =
    add_listener p "onMessage"
      (inject (Js.wrap_callback (fun msg -> f (json_stringify msg))))

  let on_disconnect (p : port) (f : unit -> unit) : unit =
    add_listener p "onDisconnect"
      (inject (Js.wrap_callback (fun _port -> f ())))
end

(* ── Runtime ─────────────────────────────────────────────────────── *)

module Runtime = struct
  let rt : _ Js.t = get chrome "runtime"

  let connect_native (app : string) : port =
    let p : _ Js.t = call rt "connectNative" [| inject (Js.string app) |] in
    Js.Unsafe.coerce p

  let get_url (path : string) : string =
    Js.to_string
      (call rt "getURL" [| inject (Js.string path) |] : Js.js_string Js.t)

  class type runtime_error = object
    method message : Js.js_string Js.t Js.readonly_prop
  end

  let last_error () : string option =
    match get_opt rt "lastError" with
    | None -> None
    | Some err ->
      Some (Js.to_string (Js.Unsafe.coerce err : runtime_error Js.t)##.message)

  let send_message (msg_json : string)
      ~(on_response : string -> string -> unit) : unit =
    call rt "sendMessage"
      [| inject (json_parse msg_json);
         inject
           (Js.wrap_callback (fun response ->
              let err =
                match last_error () with
                | Some e -> e
                | None -> ""
              in
              let resp_str = safe_stringify response in
              on_response err resp_str)) |]

  let on_installed (f : unit -> unit) : unit =
    add_listener rt "onInstalled"
      (inject (Js.wrap_callback (fun _details -> f ())))

  let on_startup (f : unit -> unit) : unit =
    add_listener rt "onStartup"
      (inject (Js.wrap_callback (fun _unit -> f ())))

  let on_message (f : string -> (string -> unit) -> unit) : unit =
    add_listener rt "onMessage"
      (inject
         (Js.wrap_callback (fun msg _sender send_response ->
              let msg_str = json_stringify msg in
              let respond s =
                ignore
                  (Js.Unsafe.fun_call send_response
                     [| inject (json_parse s) |] : unit)
              in
              f msg_str respond;
              Js._true)))

  let connect () : port =
    let p : _ Js.t = call rt "connect" [||] in
    Js.Unsafe.coerce p

  let on_connect (f : port -> unit) : unit =
    add_listener rt "onConnect"
      (inject (Js.wrap_callback (fun (p : _ Js.t) ->
        f (Js.Unsafe.coerce p : port))))
end

(* ── Tabs ────────────────────────────────────────────────────────── *)

module Tabs = struct
  let tabs : _ Js.t = get chrome "tabs"

  let create_url (url : string) : unit =
    call tabs "create" [| inject (js_obj [ ("url", inject (Js.string url)) ]) |]

  let remove (tab_id : int) : unit =
    call tabs "remove" [| inject tab_id |]

  let query_active ~(on_result : string -> int -> unit) : unit =
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
end

(* ── Action (toolbar icon) ────────────────────────────────────────── *)

module Action = struct
  let action : _ Js.t = get chrome "action"

  let set_icon (path_16 : string) (path_48 : string) (path_128 : string) : unit =
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

  let create_popup ~(url : string) ~(width : int) ~(height : int) : unit =
    call windows "create"
      [| inject
           (js_obj
              [
                ("url", inject (Js.string url));
                ("type", inject (Js.string "popup"));
                ("width", inject width);
                ("height", inject height);
              ]) |]
end

(* ── Storage ─────────────────────────────────────────────────────── *)

module Storage = struct
  let local () : _ Js.t = get (get chrome "storage") "local"

  let get_local (keys : string list)
      ~(on_result : (string * string) list -> unit) : unit =
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

  let set_local (items : (string * string) list)
      ~(on_done : unit -> unit) : unit =
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

  let create ~(id : string) ~(title : string)
      ~(contexts : string list) : unit =
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

  let create_child ~(id : string) ~(parent_id : string)
      ~(title : string) ~(contexts : string list) ?(enabled = true) () : unit =
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

  let remove_all (f : unit -> unit) : unit =
    call (menus ()) "removeAll" [| inject (Js.wrap_callback f) |]

  class type click_info = object
    method menuItemId : Js.js_string Js.t Js.readonly_prop
    method linkUrl : Js.js_string Js.t Js.Optdef.t Js.readonly_prop
    method pageUrl : Js.js_string Js.t Js.Optdef.t Js.readonly_prop
  end

  class type tab_info = object
    method id : int Js.Optdef.t Js.readonly_prop
  end

  let on_clicked (f : string -> string -> string -> int option -> unit) : unit =
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

  let on_before_navigate (f : string -> int -> int -> unit) : unit =
    add_listener (nav ()) "onBeforeNavigate"
      (inject
         (Js.wrap_callback (fun (details : nav_details Js.t) ->
              f (Js.to_string details##.url) details##.tabId details##.frameId)))
end

(* ── Navigator (browser brand detection) ─────────────────────────── *)

module Navigator = struct
  let is_grease_brand (b : string) : bool =
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

  let brand_from_user_agent (ua : string) : string =
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

  let brand_from_ua_data (uad : user_agent_data Js.t) : string =
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

  let get_browser_brand () : string =
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
