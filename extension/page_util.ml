open! Base
open! Stdio
open Js_of_ocaml

let ( let* ) = Lwt.bind

(* -- Chrome API wrappers -- *)

let send_message msg ~on_response =
  Chrome_api.Runtime.send_message
    (Yojson.Safe.to_string msg)
    ~on_response:(fun err resp_str ->
      match String.is_empty err with
      | false -> on_response (Error err)
      | true ->
        (match Yojson.Safe.from_string resp_str with
         | json -> on_response (Ok json)
         | exception _ -> on_response (Error "invalid JSON response")))

let send_protocol_command : type req resp. (req, resp) Protocol.command -> req ->
    on_response:((resp, string) Result.t -> unit) -> unit =
  fun cmd params ~on_response ->
    let frame = Protocol.make_request_frame cmd params 0 in
    let msg = Protocol.frame_to_yojson frame in
    send_message msg ~on_response:(fun result ->
      match result with
      | Error e -> on_response (Error e)
      | Ok json ->
        let parsed =
          Result.bind (Protocol.frame_of_yojson json) ~f:(fun frame ->
            match Protocol.parse_response_payload frame with
            | Ok (Protocol.Success payload) -> Protocol.response_deserializer cmd payload
            | Ok (Protocol.Failure msg) -> Error msg
            | Error msg -> Error msg)
        in
        on_response parsed)

let storage_get keys ~on_result =
  Chrome_api.Storage.get_local keys ~on_result

let storage_set items ~on_done =
  Chrome_api.Storage.set_local items ~on_done

let create_tab url =
  Chrome_api.Tabs.create_url url

let get_extension_url path =
  Chrome_api.Runtime.get_url path

let query_active_tab ~on_result =
  Chrome_api.Tabs.query_active ~on_result

let internal_url_prefixes =
  [ "chrome://"; "chrome-extension://"; "about:"; "edge://"; "brave://";
    "chrome-search://"; "devtools://" ]

let is_internal_url url =
  List.exists internal_url_prefixes ~f:(fun prefix -> String.is_prefix url ~prefix)

let validate_regexp pattern =
  match Regexp.regexp pattern with
  | _ -> Ok ()
  | exception Js_error.Exn e -> Error (Js_error.message e)
  | exception _ -> Error "invalid pattern"

(* -- DOM helpers -- *)

let get_by_id id : Dom_html.element Js.t =
  Dom_html.getElementById id

let input_by_id id : Dom_html.inputElement Js.t =
  let el = Dom_html.getElementById id in
  Js.Opt.get (Dom_html.CoerceTo.input el)
    (fun () -> failwith (Printf.sprintf "Element '%s' is not an input" id))

let select_by_id id : Dom_html.selectElement Js.t =
  let el = Dom_html.getElementById id in
  Js.Opt.get (Dom_html.CoerceTo.select el)
    (fun () -> failwith (Printf.sprintf "Element '%s' is not a select" id))

let set_text (el : Dom_html.element Js.t) text =
  el##.textContent := Js.some (Js.string text)

let set_html (el : Dom_html.element Js.t) html =
  el##.innerHTML := Js.string html

let on_click (el : Dom_html.element Js.t) f =
  el##.onclick := Dom_html.handler (fun _ev -> f (); Js._true)

let set_timeout = Chrome_api.set_timeout

let set_display (el : Dom_html.element Js.t) value =
  el##.style##.display := Js.string value

let set_class (el : Dom_html.element Js.t) cls =
  el##.className := Js.string cls

let set_disabled (el : Dom_html.element Js.t) disabled =
  match disabled with
  | true -> el##setAttribute (Js.string "disabled") (Js.string "")
  | false -> el##removeAttribute (Js.string "disabled")

let add_class (el : Dom_html.element Js.t) cls =
  let current = Js.to_string el##.className in
  match String.is_substring current ~substring:cls with
  | true -> ()
  | false -> el##.className := Js.string (current ^ " " ^ cls)

let remove_class (el : Dom_html.element Js.t) cls =
  let current = Js.to_string el##.className in
  String.split current ~on:' '
  |> List.filter ~f:(fun c -> not (String.equal c cls))
  |> String.concat ~sep:" "
  |> fun s -> el##.className := Js.string s

let escape_html s =
  String.concat_map s ~f:(fun c ->
    match c with
    | '&' -> "&amp;"
    | '<' -> "&lt;"
    | '>' -> "&gt;"
    | '"' -> "&quot;"
    | '\'' -> "&#39;"
    | c -> String.of_char c)

let escape_regexp s =
  String.concat_map s ~f:(fun c ->
    match c with
    | '.' | '*' | '+' | '?' | '^' | '$'
    | '{' | '}' | '(' | ')' | '|' | '[' | ']' | '\\' ->
      Printf.sprintf "[%c]" c
    | c -> String.of_char c)

let bind_clicks (parent : Dom_html.element Js.t) ~selector ~attr ~f =
  let nodes = parent##querySelectorAll (Js.string selector) in
  let len = nodes##.length in
  List.init len ~f:(fun i ->
    Js.Opt.get (nodes##item i) (fun () -> assert false))
  |> List.iter ~f:(fun btn ->
    (btn :> Dom_html.element Js.t)##.onclick :=
      Dom_html.handler (fun _ev ->
        let v =
          Js.Opt.case
            ((btn :> Dom.element Js.t)##getAttribute (Js.string attr))
            (fun () -> "")
            Js.to_string
        in
        f v;
        Js._true))

let get_search_param key =
  let search = Js.to_string Dom_html.window##.location##.search in
  match String.is_prefix search ~prefix:"?" with
  | false -> None
  | true ->
    String.drop_prefix search 1
    |> String.split ~on:'&'
    |> List.find_map ~f:(fun param ->
      match String.lsplit2 param ~on:'=' with
      | Some (k, v) when String.equal k key ->
        Some (Js.to_string (Js.decodeURIComponent (Js.string v)))
      | _ -> None)

let url_origin url_str =
  match String.substr_index url_str ~pattern:"://" with
  | None -> None
  | Some scheme_end ->
    let after_scheme = scheme_end + 3 in
    let path_start =
      match String.substr_index url_str ~pattern:"/" ~pos:after_scheme with
      | Some i -> i
      | None -> String.length url_str
    in
    Some (String.prefix url_str path_start)

let create_option (doc : Dom_html.document Js.t) ~value ~text ~selected =
  let opt = Dom_html.createOption doc in
  opt##.value := Js.string value;
  opt##.textContent := Js.some (Js.string text);
  opt##.selected := Js.bool selected;
  opt

(* -- Port-based Client connection for popup pages -- *)

let connect_port ~name ~on_ready ~on_event =
  Chrome_api.log "connect_port: calling chrome.runtime.connect()";
  let port = Chrome_api.Runtime.connect () in
  Chrome_api.log "connect_port: port created, setting up listeners";
  let write msg = Chrome_api.Port.post_message_json port msg in
  let (read, push_incoming) = Lwt_stream.create () in
  Chrome_api.Port.on_message_json port (fun msg ->
    Chrome_api.log (Printf.sprintf "connect_port: incoming message: %s"
      (String.prefix msg 200));
    push_incoming (Some msg));
  Chrome_api.Port.on_disconnect port (fun () ->
    Chrome_api.log "connect_port: port disconnected";
    push_incoming None);
  Lwt.async (fun () ->
    Chrome_api.log "connect_port: starting Client.init";
    Lwt.catch
      (fun () ->
        let* (conn, events) = Client.init ~write ~read ~name () in
        Chrome_api.log "connect_port: Client.init complete, connected";
        on_ready conn;
        let rec forward () =
          let* ev = Lwt_stream.next events in
          on_event ev;
          forward ()
        in
        Lwt.catch forward (fun _exn -> Lwt.return_unit))
      (fun exn ->
        Chrome_api.log (Printf.sprintf "connect_port: error: %s"
          (Base.Exn.to_string exn));
        Lwt.return_unit))
