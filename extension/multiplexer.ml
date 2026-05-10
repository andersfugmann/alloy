open! Base
open! Stdio
open! Lwt.Syntax

(* This is just a simple bridge to a client over Chrome channels *)
(* TODO: Rename to port_multiplexer *)

let log = Chrome_api.log

let is_closed close_var =
  Lwt_mvar.is_empty close_var |> not

let close close_var =
  Lwt.async (fun () ->
      match is_closed close_var with
      | false -> Lwt_mvar.put close_var ()
      | true -> Lwt.return ()
    )

let handle_broadcast port close_var push =
  let () = match is_closed close_var with
    | true ->
      raise Lwt_stream.Closed
    | false -> ()
  in
  match push with
  | Some push ->
    Protocol.make_push_frame push
    |> Protocol.raw_frame_to_yojson
    |> Chrome_api.Port.post_message port
  | None -> close close_var

let handle_response_payload port close_var correlation_id payload =
  let payload =
    match payload with
    | Some payload -> payload
    | None ->
      close close_var;
      Protocol.response_payload_to_yojson (Error "Closed")
  in
  Protocol.{ correlation_id; payload }
  |> Protocol.raw_frame_to_yojson
  |> Chrome_api.Port.post_message port

let handle_receive client port close_var json =
  let frame =
    Protocol.raw_frame_of_yojson json
    |> Result.ok_or_failwith (* This is intended: Crash on broken invariant *)
  in
  Client.proxy client frame.payload (handle_response_payload port close_var frame.correlation_id)

let handle_message push json =
  Protocol.raw_frame_of_yojson json
  |> (function
      | Ok json -> Some json
      | Error s ->
        log (Printf.sprintf "Error parsing frame: %s" s);
        None
    )
  |> push

let handle_disconnect close_var () =
  close close_var

let handle_connect client port =
  let close_var = Lwt_mvar.create_empty () in
  Client.register_broadcast client (handle_broadcast port close_var);
  Chrome_api.Port.on_message port (handle_receive client port close_var);
  Chrome_api.Port.on_disconnect port (handle_disconnect close_var);
  Lwt.async (fun () ->
      let* () = Client.closed client in
      Chrome_api.Port.disconnect port;
      Lwt.return_unit
    );
  ()

type event = Closed | Connect of Chrome_api.port

type t = { stream: event Lwt_stream.t; push: event -> unit }

let init () =
  let stream, push = Lwt_stream.create () in
  let push e = push (Some e) in
  Chrome_api.Port.on_connect (fun port -> push (Connect port));
  { stream; push }

let rec consume_events stream client =
  let* event = Lwt_stream.next stream in
  match event with
  | Closed -> Lwt.return_unit
  | Connect port ->
    handle_connect client port;
    consume_events stream client

(** Start the multiplexer using the given client *)
let start t client =
  Lwt.async (fun () ->
      let* () = Client.closed client in
      t.push Closed;
      Lwt.return_unit
    );
  Lwt.async (fun () -> consume_events t.stream client);
  ()

(* Create a client. *)
let create_client () =
  let port = Chrome_api.Port.connect () in

  let recv_s, push = Lwt_stream.create () in
  (* Disconnect recv_s *)
  Chrome_api.Port.on_disconnect port (fun () -> push (None));

  let send_f port frame =
    Protocol.raw_frame_to_yojson frame
    |> Chrome_api.Port.post_message port
  in
  Chrome_api.Port.on_message port (handle_message push);
  let send_f = send_f port in
  Client.init ~recv_s ~send_f ()
