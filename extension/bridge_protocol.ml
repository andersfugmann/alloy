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

(* -- Connection state (mutable: shared between caller and port handler) *)

type connection = {
  mutable next_id : int;
  mutable pending : (Protocol.response_envelope Lwt.u) Map.M(Int).t;
  send_raw : string -> unit;
}

let create ~send_raw =
  { next_id = 1; pending = Map.empty (module Int); send_raw }

(* -- Sending *)

let send_raw_envelope (conn : connection) ~(command : string)
    ~(params : Yojson.Safe.t) : Protocol.response_envelope Lwt.t =
  let id = conn.next_id in
  let env : Protocol.request_envelope = { id; command; params; tenant = None } in
  let (promise, resolver) = Lwt.wait () in
  conn.next_id <- id + 1;
  conn.pending <- Map.set conn.pending ~key:id ~data:resolver;
  conn.send_raw (json_to_string (Protocol.request_envelope_to_yojson env));
  promise

let call : type req resp. connection -> (req, resp) Protocol.command -> req ->
    (resp, string) Result.t Lwt.t =
  fun conn cmd request ->
    let command = Protocol.command_name cmd in
    let params = Protocol.request_serializer cmd request in
    let* resp_env = send_raw_envelope conn ~command ~params in
    match resp_env.success with
    | true -> Lwt.return (Protocol.response_deserializer cmd resp_env.payload)
    | false -> Lwt.return (Error (Option.value resp_env.error ~default:"unknown error"))

(* -- Receiving *)

type incoming =
  | Push of Protocol.push
  | Response of { id : int; envelope : Protocol.response_envelope }
  | Parse_error of string

let parse_message (raw : string) : incoming =
  match json_of_string raw with
  | Error msg -> Parse_error (Printf.sprintf "invalid JSON: %s" msg)
  | Ok json ->
    match Protocol.server_message_of_yojson json with
    | Ok (Push { id = _; push }) -> Push push
    | Ok (Response env) -> Response { id = env.id; envelope = env }
    | Error msg -> Parse_error (Printf.sprintf "invalid message: %s" msg)

let dispatch_response (conn : connection) (id : int)
    (envelope : Protocol.response_envelope) : bool =
  match Map.find conn.pending id with
  | None -> false
  | Some resolver ->
    conn.pending <- Map.remove conn.pending id;
    Lwt.wakeup_later resolver envelope;
    true
