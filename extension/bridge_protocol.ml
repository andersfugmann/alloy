open! Base
open! Stdio

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

(* -- Connection state *)

type connection = {
  next_id : int;
  pending : (Protocol.response_envelope -> unit) Map.M(Int).t;
  send_raw : string -> unit;
}

let empty_connection ~send_raw =
  { next_id = 1; pending = Map.empty (module Int); send_raw }

(* -- Sending *)

let send_envelope (conn : connection) ~(command : string)
    ~(params : Yojson.Safe.t)
    ~(on_response : Protocol.response_envelope -> unit) : connection =
  let id = conn.next_id in
  let env : Protocol.request_envelope = { id; command; params; tenant = None } in
  conn.send_raw (json_to_string (Protocol.request_envelope_to_yojson env));
  { conn with
    next_id = id + 1;
    pending = Map.set conn.pending ~key:id ~data:on_response }

let send_command : type req resp. connection -> (req, resp) Protocol.command -> req ->
    ((resp, string) Result.t -> unit) -> connection =
  fun conn cmd request on_result ->
    let command = Protocol.command_name cmd in
    let params = Protocol.request_serializer cmd request in
    send_envelope conn ~command ~params ~on_response:(fun resp_env ->
      match resp_env.success with
      | true -> on_result (Protocol.response_deserializer cmd resp_env.payload)
      | false -> on_result (Error (Option.value resp_env.error ~default:"unknown error")))

let call : type req resp. connection -> (req, resp) Protocol.command -> req ->
    connection * (resp, string) Result.t Lwt.t =
  fun conn cmd request ->
    let (promise, resolver) = Lwt.wait () in
    let conn = send_command conn cmd request (fun result ->
      Lwt.wakeup_later resolver result) in
    (conn, promise)

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
    (envelope : Protocol.response_envelope) : connection * bool =
  match Map.find conn.pending id with
  | None -> (conn, false)
  | Some cb ->
    cb envelope;
    ({ conn with pending = Map.remove conn.pending id }, true)
