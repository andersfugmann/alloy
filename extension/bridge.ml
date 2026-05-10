open! Base
open! Stdio

let ( let* ) = Lwt.bind

let log = Chrome_api.log

let non_empty s =
  match String.is_empty s with
  | true -> None
  | false -> Some s

let make_client ?name ?brand ~host ~port ~debug () =
  let native_port = Chrome_api.Port.Native.connect "alloy" in
  log "Connected to native messaging host";
  let (read_stream, push_incoming) = Lwt_stream.create () in
  Chrome_api.Port.on_message native_port (fun msg ->
    push_incoming (Some msg));
  Chrome_api.Port.on_disconnect native_port (fun () ->
    push_incoming None);
  (* Send bridge handshake *)
  let addr : Protocol.listen_address = { host; port } in
  let req = Bridge_protocol.make_request ~debug addr in
  Chrome_api.Port.post_message native_port (Bridge_protocol.request_to_yojson req);
  (* Wait for bridge response *)
  let* json = Lwt_stream.next read_stream in
  let hostname =
    match Bridge_protocol.parse_response json with
    | Ok connected -> connected.hostname
    | Error _ -> ""
  in
  let name =
    match name with
    | Some _ -> name
    | None -> non_empty hostname
  in
  log (Printf.sprintf "Bridge connected: hostname=%s, tenant=%s"
    hostname (Option.value name ~default:"(default)"));
  let recv_s =
    Lwt_stream.filter_map (fun json ->
      match Protocol.raw_frame_of_yojson json with
      | Ok frame -> Some frame
      | Error _ -> None
    ) read_stream
  in
  let send_f frame =
    Protocol.raw_frame_to_yojson frame
    |> Chrome_api.Port.post_message native_port
  in
  Client.init ~recv_s ~send_f ?name ?brand ()
