open! Base
open! Stdio

let ( let* ) = Lwt.bind

(* -- JSON utilities *)

let json_of_string (s : string) : (Yojson.Safe.t, string) Result.t =
  Protocol.parse_json_string s

let json_to_string (json : Yojson.Safe.t) : string =
  Yojson.Safe.to_string json

let string_field (json : Yojson.Safe.t) (key : string) : (string, string) Result.t =
  match json with
  | `Assoc pairs ->
    (match List.Assoc.find pairs ~equal:String.equal key with
     | Some (`String s) -> Ok s
     | Some _ -> Error (Printf.sprintf "field %s: expected string" key)
     | None -> Error (Printf.sprintf "missing field: %s" key))
  | _ -> Error "expected JSON object"

(* -- Events exposed to the caller *)

type event =
  | Push of Protocol.push
  | Disconnected

(* -- Internal types *)

type pending_entry = Entry : {
  cmd : ('req, 'resp) Protocol.command;
  resolver : ('resp, string) Result.t Lwt.u;
} -> pending_entry

type outgoing = Outgoing : {
  cmd : ('req, 'resp) Protocol.command;
  request : 'req;
  resolver : ('resp, string) Result.t Lwt.u;
} -> outgoing

type raw_request = {
  command : string;
  params : Yojson.Safe.t;
  envelope_resolver : Protocol.response_envelope Lwt.u;
}

type command =
  | Typed of outgoing
  | Raw of raw_request

type loop_state = {
  next_id : int;
  pending : pending_entry Map.M(Int).t;
  raw_pending : (Protocol.response_envelope Lwt.u) Map.M(Int).t;
}

(* -- Connection (opaque to caller) *)

type connection = {
  push_command : command option -> unit;
}

(* -- Message parsing *)

type incoming =
  | Server_push of Protocol.push
  | Response of { id : int; envelope : Protocol.response_envelope }
  | Parse_error of string

let parse_message (raw : string) : incoming =
  match json_of_string raw with
  | Error msg -> Parse_error (Printf.sprintf "invalid JSON: %s" msg)
  | Ok json ->
    match Protocol.server_message_of_yojson json with
    | Ok (Push { id = _; push }) -> Server_push push
    | Ok (Response env) -> Response { id = env.id; envelope = env }
    | Error msg -> Parse_error (Printf.sprintf "invalid message: %s" msg)

(* -- Connection thread *)

let dispatch_pending (state : loop_state) (id : int)
    (envelope : Protocol.response_envelope) : loop_state =
  match Map.find state.pending id with
  | Some (Entry { cmd; resolver }) ->
    let result =
      match envelope.success with
      | true -> Protocol.response_deserializer cmd envelope.payload
      | false -> Error (Option.value envelope.error ~default:"unknown error")
    in
    Lwt.wakeup_later resolver result;
    { state with pending = Map.remove state.pending id }
  | None ->
    match Map.find state.raw_pending id with
    | Some resolver ->
      Lwt.wakeup_later resolver envelope;
      { state with raw_pending = Map.remove state.raw_pending id }
    | None -> state

let handle_incoming (state : loop_state) (raw : string)
    ~(push_event : event option -> unit) : loop_state =
  match parse_message raw with
  | Parse_error _msg -> state
  | Server_push p ->
    push_event (Some (Push p));
    state
  | Response { id; envelope } ->
    dispatch_pending state id envelope

let handle_command (state : loop_state) (cmd : command)
    ~(send_raw : string -> unit) : loop_state =
  let id = state.next_id in
  match cmd with
  | Typed (Outgoing { cmd = c; request; resolver }) ->
    let env : Protocol.request_envelope = {
      id;
      command = Protocol.command_name c;
      params = Protocol.request_serializer c request;
      tenant = None;
    } in
    send_raw (json_to_string (Protocol.request_envelope_to_yojson env));
    { next_id = id + 1;
      pending = Map.set state.pending ~key:id ~data:(Entry { cmd = c; resolver });
      raw_pending = state.raw_pending }
  | Raw { command; params; envelope_resolver } ->
    let env : Protocol.request_envelope = { id; command; params; tenant = None } in
    send_raw (json_to_string (Protocol.request_envelope_to_yojson env));
    { next_id = id + 1;
      pending = state.pending;
      raw_pending = Map.set state.raw_pending ~key:id ~data:envelope_resolver }

let run_loop ~(command_stream : command Lwt_stream.t)
    ~(incoming_stream : string Lwt_stream.t)
    ~(push_event : event option -> unit)
    ~(send_raw : string -> unit) : unit Lwt.t =
  let initial_state = {
    next_id = 1;
    pending = Map.empty (module Int);
    raw_pending = Map.empty (module Int);
  } in
  let rec loop state =
    let* msg = Lwt.pick [
      (let* c = Lwt_stream.next command_stream in Lwt.return (`Command c));
      (let* raw = Lwt_stream.next incoming_stream in Lwt.return (`Incoming raw));
    ] in
    match msg with
    | `Command cmd ->
      loop (handle_command state cmd ~send_raw)
    | `Incoming raw ->
      loop (handle_incoming state raw ~push_event)
  in
  loop initial_state

(* -- Public API *)

let init ~(send_raw : string -> unit) ~(incoming : string Lwt_stream.t)
    ~(register : Protocol.register_request) : connection * event Lwt_stream.t =
  let (command_stream, push_command) = Lwt_stream.create () in
  let (event_stream, push_event) = Lwt_stream.create () in
  let conn = { push_command } in
  (* Send registration as id=0, fire-and-forget (server confirms via Registered push) *)
  let register_env = Protocol.make_request_envelope Register register 0 None in
  send_raw (json_to_string (Protocol.request_envelope_to_yojson register_env));
  (* Start connection thread *)
  Lwt.async (fun () ->
    Lwt.catch
      (fun () -> run_loop ~command_stream ~incoming_stream:incoming ~push_event ~send_raw)
      (fun _exn ->
        push_event (Some Disconnected);
        Lwt.return_unit));
  (conn, event_stream)

let call : type req resp. connection -> (req, resp) Protocol.command -> req ->
    (resp, string) Result.t Lwt.t =
  fun conn cmd request ->
    let (promise, resolver) = Lwt.wait () in
    conn.push_command (Some (Typed (Outgoing { cmd; request; resolver })));
    promise

let send_raw_envelope (conn : connection) ~(command : string)
    ~(params : Yojson.Safe.t) : Protocol.response_envelope Lwt.t =
  let (promise, resolver) = Lwt.wait () in
  conn.push_command (Some (Raw { command; params; envelope_resolver = resolver }));
  promise
