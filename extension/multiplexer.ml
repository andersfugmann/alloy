open! Base
open! Stdio

(* This is just a simple bridge to a client over Chrome channels *)
(* TODO: Rename to port_multiplexer *)

let is_closed close_var =
  Lwt_mvar.is_empty close_var |> not

let close close_var =
  Lwt.async (fun () ->
      match is_closed close_var with
      | false -> Lwt_mvar.put close_var ()
      | true -> Lwt.return ()
    )

let handle_broadcast port close_var broadcast =
  let () = match is_closed close_var with
    | true -> raise Lwt_stream.Closed
    | false -> ()
  in
  match broadcast with
  | Some broadcast ->
    Protocol.make_push_frame broadcast
    |> Protocol.frame_to_yojson
    |> Yojson.Safe.to_string
    |> Chrome_api.Port.post_message_json port
  | None -> close close_var

let handle_response_payload port close_var id payload =
  let payload =
    match payload with
    | Some payload -> payload
    | None ->
      close close_var; (* The var will not be closed untill this function ends. There are not scheduling points, its not a problem*)
      Protocol.Failure "Closed" |> Protocol.response_payload_to_yojson
  in
  Protocol.{ id; payload }
  |> Protocol.frame_to_yojson
  |> Yojson.Safe.to_string
  |> Chrome_api.Port.post_message_json port

let handle_receive client port close_var json_str =
  let frame =
    Yojson.Safe.from_string json_str
    |> Protocol.frame_of_yojson
    |> Result.ok_or_failwith (* This is intended: Crash on broken invariant *)
  in
  Client.proxy client frame.payload (handle_response_payload port close_var frame.id)

let handle_message push json_string =
  Yojson.Safe.from_string json_string
  |> Protocol.frame_of_yojson
  |> (function
      | Ok json -> Some json
      | Error _s ->
        (* Close the connection on error! *)
        (* TODO: log error *)
        None
    )
  |> push

let handle_disconnect close_var () =
  close close_var

let handle_connect client port =
  let close_var = Lwt_mvar.create_empty () in
  Client.register_broadcast client (handle_broadcast port close_var);
  Chrome_api.Port.on_message_json port (handle_receive client port close_var);
  Chrome_api.Port.on_disconnect port (handle_disconnect close_var);
  ()



(** Start the multiplexer using the given client *)
let start client =
  Chrome_api.Runtime.on_connect (handle_connect client)

(* Create a client. *)
let create_client () =
  let port = Chrome_api.Runtime.connect () in

  (* Need a recv_s and send_f *)
  let recv_s, push = Lwt_stream.create () in

  let send_f port frame =
    Protocol.frame_to_yojson frame
    |> Yojson.Safe.to_string
    |> Chrome_api.Port.post_message_json port
  in
  let send_f = send_f port in

  Chrome_api.Port.on_message_json port (handle_message push);

  Client.init ~recv_s ~send_f ()
