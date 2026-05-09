open! Base
open! Stdio
open! Lwt.Syntax

type message =
  | Close
  | Packet of Protocol.frame
  | Request of Protocol.json * (Protocol.json option -> unit)
  | Register of (Protocol.push option -> unit)

type state = {
  tenant: string;
  next_id: int;
  pending: (int * (Protocol.json option -> unit)) list;
  listeners: (Protocol.push option -> unit) list;
}

type t = { push: (message option -> unit); tenant: string; closed: unit Lwt.t; signal_closed: unit Lwt.u }

let is_registration_request payload =
  let cmd = Protocol.command_name Register in
  match Protocol.request_payload_of_yojson payload with
  | Ok { Protocol.command; _} -> String.equal command cmd
  | Error _ -> false

let close_clients state =
    List.iter ~f:(fun listener -> listener None) state.listeners;
    List.iter ~f:(fun (_, receiver) -> receiver None) state.pending;
    { state with pending = []; listeners = [] }

let handle_message ~send_f state = function
  | Close ->
    close_clients state
  | Packet { id=0; payload } ->
    let push_msg = match Protocol.push_of_yojson payload with
      | Ok push -> Some push
      | Error _s ->
        let _ = close_clients state in
        failwith "Could not decode push payload"
    in
    (* Send packets to all listeners. Any listener that errors should be removed *)
    let listeners =
      List.fold ~init:[] ~f:(fun acc handler ->
          match handler push_msg with
          | () -> handler :: acc
          | exception _ -> acc
        ) state.listeners
    in
    { state with listeners }
  | Packet { id; payload } ->
    let pending = match List.Assoc.find state.pending ~equal:Int.equal id with
      | None -> state.pending
      | Some handler ->
        handler (Some payload);
        List.Assoc.remove state.pending ~equal:Int.equal id
    in
    { state with pending }
  | Request (req, rep_handler) when is_registration_request req ->
    let reply =
      Protocol.(Registered { tenant_id = state.tenant })
      |> Protocol.push_to_yojson
    in
    rep_handler (Some reply);
    state

  | Request (req, rep_handler) ->
    send_f Protocol.{ id = state.next_id; payload = req };
    let pending = (state.next_id, rep_handler) :: state.pending in
    { state with next_id = state.next_id + 1; pending }

  | Register listener ->
    { state with listeners = listener :: state.listeners }

let is_closed t =
  match Lwt.state t.closed with
  | Lwt.Return () -> true
  | _ -> false

let close t =
  match is_closed t with
  | true -> ()
  | false ->
    t.push (Some Close);
    t.push None;
    Lwt.wakeup t.signal_closed ()

let init ~recv_s ~send_f ?name ?brand () =
  (* Create a stream for receiving events *)
  let stream, push = Lwt_stream.create () in
  let register_req = Protocol.{ brand; address = None; name } in
  let frame = Protocol.make_request_frame Register register_req 0 in
  send_f frame;
  (* Wait for registered reply *)
  let* frame = Lwt_stream.next recv_s in
  let tenant =
    match Protocol.push_of_yojson frame.Protocol.payload with
    | Ok (Registered { tenant_id }) ->
      tenant_id
    | _ ->
      failwith "Unexpected packet received while waiting for registration reply"
  in
  let closed, signal_closed = Lwt.wait () in
  let t = { push; tenant; closed; signal_closed } in
  (* Register for broadcasts *)
  let broadcast_stream, broadcast_push = Lwt_stream.create () in
  push (Some (Register broadcast_push));
  Lwt.async (fun () ->
      let* () = Lwt_stream.iter (fun frame -> push (Some (Packet frame))) recv_s in
      Lwt.return (close t)
    );
  Lwt.async (fun () ->
      Lwt_stream.fold
        (fun msg state -> handle_message ~send_f state msg)
        stream
        { next_id = 1; pending = []; listeners = []; tenant }
      |> Lwt.map (fun _ -> close t; ())
    );
  Lwt.return (t, broadcast_stream)

(* Send messages though the client from another client *)
(* This takes request payload and return a response payload *)
let proxy t payload handler =
  t.push (Some (Request (payload, handler)))

let call : type req resp. t -> (req, resp) Protocol.command -> req ->
  (resp, string) Result.t Lwt.t = fun client cmd request ->
  let (promise, resolver) = Lwt.wait () in
  let payload =
    Protocol.request_payload_to_yojson
      { command = Protocol.command_name cmd;
        params = Protocol.request_serializer cmd request }
  in
  let response_handler json =
    let response =
      let (let*) a f = Result.bind ~f a in
      let* json = Result.of_option json ~error:"Closed" in
      let* payload = Protocol.response_payload_of_yojson json in
      let* payload = match payload with
        | Success json -> Result.return json
        | Failure s -> Result.fail s
      in
      Protocol.response_deserializer cmd payload
    in
    Lwt.wakeup resolver response
  in
  proxy client payload response_handler;
  promise


let register_broadcast t callback =
  t.push (Some (Register callback))

let closed t = t.closed
let name t = t.tenant

(** Helper functions to connect a client to a client *)
let make_proxy_client t =
  let recv_s, push = Lwt_stream.create () in
  let handle_broadcast msg =
    let frame = Option.map msg ~f:Protocol.make_push_frame in
    push frame
  in
  register_broadcast t handle_broadcast;
  (* Sending requests though another client will require us to match frame id's *)
  (* send and recv has frame ids. *)

  let send_f frame =
    let handler payload =
      let frame = Option.map ~f:(fun payload -> Protocol.{ id = frame.id; payload }) payload in
      push frame
    in
    proxy t frame.payload handler
  in
  init ~recv_s ~send_f ()

(* TODO. Create test cases for the proxy. Need longer daisy chain, and verify that broadcast work Tests should not be in this file *)
