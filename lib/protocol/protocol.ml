open !Base
open !Stdio

(* -- Core data types *)

type tenant_id = string [@@deriving yojson]
type url = string [@@deriving yojson]

type rule = {
  pattern : string;
  target : tenant_id;
  enabled : bool;
}
[@@deriving yojson]

let rules_to_yojson rules =
  `List (List.map rules ~f:rule_to_yojson)

let rules_of_yojson = function
  | `List items ->
    items
    |> List.map ~f:rule_of_yojson
    |> Result.all
  | _ -> Error "rules: expected JSON array"

type tenant_config = {
  browser_cmd : string option; [@default None]
  label : string;
  color : string;
  brand : string option; [@default None]
}
[@@deriving yojson]

type defaults = {
  unmatched : string;
  cooldown_seconds : int;
  browser_launch_timeout : int;
}
[@@deriving yojson]

type tenants = (string * tenant_config) list
let tenants_to_yojson lst =
  `Assoc (List.map lst ~f:(fun (k, v) -> (k, tenant_config_to_yojson v)))

let tenants_of_yojson = function
  | `Assoc pairs ->
    pairs
    |> List.map ~f:(fun (k, v) ->
         tenant_config_of_yojson v |> Result.map ~f:(fun tc -> (k, tc)))
    |> Result.all
  | _ -> Error "tenants: expected JSON object"

type listen_address = {
  host : string;
  port : int;
}
[@@deriving yojson]

type config = {
  listen : listen_address list;
  allowed_networks : Cidr.t list;
  tenants : tenants;
  defaults : defaults;
}
[@@deriving yojson]

type status_info = {
  registered_tenants : tenant_id list;
  uptime_seconds : int;
}
[@@deriving yojson]

(* -- Response payload types *)

type route_result =
  | Local
  | Remote of tenant_id
[@@deriving yojson]

type test_result =
  | Match of { tenant : tenant_id; rule_index : int }
  | No_match of { default_tenant : tenant_id }
[@@deriving yojson]

(* -- Request types *)

type register_request = {
  brand : string option; [@default None]
  address : string option; [@default None]
  name : string option; [@default None]
}
[@@deriving yojson]

type open_request = { url : string } [@@deriving yojson]
type open_on_request = { target : string; url : string } [@@deriving yojson]

(* -- GADT command type *)

type (_, _) command =
  | Register : (register_request, string) command
  | Open : (open_request, route_result) command
  | Open_on : (open_on_request, route_result) command
  | Test : (open_request, test_result) command
  | Get_config : (unit, config) command
  | Set_config : (config, unit) command
  | Get_rules : (unit, rule list) command
  | Set_rules : (rule list, unit) command
  | Status : (unit, status_info) command

(* -- Helpers *)

let ( let* ) r f = Result.bind r ~f

let parse_json_string (s : string) : (Yojson.Safe.t, string) Result.t =
  match Yojson.Safe.from_string s with
  | json -> Result.return json
  | exception Yojson.Json_error msg -> Result.failf "invalid JSON: %s" msg

(* -- JSON identity for embedding raw JSON in wire envelopes *)

type json = Yojson.Safe.t
let json_to_yojson (x : json) : Yojson.Safe.t = x
let json_of_yojson (x : Yojson.Safe.t) : (json, string) Result.t = Ok x

(* -- Wire envelope types *)

type request_envelope = {
  id : int;
  command : string;
  params : json; [@default `Null]
  tenant : string option; [@default None]
}
[@@deriving yojson]

type response_envelope = {
  id : int;
  success : bool;
  payload : json; [@default `Null]
  error : string option; [@default None]
}
[@@deriving yojson]

(* -- Push messages *)

type push =
  | Navigate of { url : string }
  | Registered of { tenant_id : string }
  | Config_updated of { config : config; registered_tenants : string list }
[@@deriving yojson]

type server_message =
  | Response of response_envelope
  | Push of { id : int; push : push }
[@@deriving yojson]

(* -- Command serialization *)

let command_name : type req resp. (req, resp) command -> string = function
  | Register -> "register"
  | Open -> "open"
  | Open_on -> "open_on"
  | Test -> "test"
  | Get_config -> "get_config"
  | Set_config -> "set_config"
  | Get_rules -> "get_rules"
  | Set_rules -> "set_rules"
  | Status -> "status"

let request_serializer : type req resp. (req, resp) command -> (req -> Yojson.Safe.t) = function
  | Register -> register_request_to_yojson
  | Open -> open_request_to_yojson
  | Open_on -> open_on_request_to_yojson
  | Test -> open_request_to_yojson
  | Get_config -> (fun () -> `Null)
  | Set_config -> config_to_yojson
  | Get_rules -> (fun () -> `Null)
  | Set_rules -> rules_to_yojson
  | Status -> (fun () -> `Null)

let response_deserializer : type req resp. (req, resp) command -> (Yojson.Safe.t -> (resp, string) Result.t) = function
  | Register ->
    (fun payload -> match payload with
     | `String s -> Ok s
     | _ -> Error "register: expected string")
  | Open -> route_result_of_yojson
  | Open_on -> route_result_of_yojson
  | Test -> test_result_of_yojson
  | Get_config -> config_of_yojson
  | Set_config -> (fun _ -> Ok ())
  | Get_rules -> rules_of_yojson
  | Set_rules -> (fun _ -> Ok ())
  | Status -> status_info_of_yojson

(* -- High-level serialization *)

let make_request_envelope : type req resp. (req, resp) command -> req -> int -> string option -> request_envelope =
  fun cmd request id tenant ->
  { id; command = command_name cmd; params = request_serializer cmd request; tenant }

let serialize_request_envelope env =
  request_envelope_to_yojson env |> Yojson.Safe.to_string

let deserialize_request_envelope str =
  let* json = parse_json_string str in
  request_envelope_of_yojson json

let make_response_envelope id result =
  match result with
  | Ok json -> { id; success = true; payload = json; error = None }
  | Error msg -> { id; success = false; payload = `Null; error = Some msg }

let serialize_server_message msg =
  server_message_to_yojson msg |> Yojson.Safe.to_string

let deserialize_server_message str =
  let* json = parse_json_string str in
  server_message_of_yojson json

(* -- Inline expect tests *)

let%expect_test "request envelope round-trip" =
  let test : type req resp. (req, resp) command -> req -> string -> unit =
    fun cmd params label ->
      let env = make_request_envelope cmd params 0 None in
      let json_str = serialize_request_envelope env in
      match deserialize_request_envelope json_str with
      | Ok env2 ->
        (match String.equal env2.command (command_name cmd) with
         | true -> printf "%s: ok\n" label
         | false -> printf "%s: FAIL command mismatch\n" label)
      | Error e -> printf "%s: FAIL %s\n" label e
  in
  test Register { brand = Some "Chrome"; address = None; name = None } "register";
  test Open { url = "https://x.com" } "open";
  test Open_on { target = "work"; url = "https://x.com" } "open_on";
  test Test { url = "https://test.com" } "test";
  test Get_config () "get_config";
  test Get_rules () "get_rules";
  test Set_rules [] "set_rules";
  test Status () "status";
  [%expect {|
    register: ok
    open: ok
    open_on: ok
    test: ok
    get_config: ok
    get_rules: ok
    set_rules: ok
    status: ok
    |}]

(* Test helper: serialize a response for a command *)
let response_serializer : type req resp. (req, resp) command -> (resp -> Yojson.Safe.t) = function
  | Register -> (fun s -> `String s)
  | Open -> route_result_to_yojson
  | Open_on -> route_result_to_yojson
  | Test -> test_result_to_yojson
  | Get_config -> config_to_yojson
  | Set_config -> (fun () -> `Null)
  | Get_rules -> rules_to_yojson
  | Set_rules -> (fun () -> `Null)
  | Status -> status_info_to_yojson

let%expect_test "response round-trip" =
  let test : type req resp. (req, resp) command -> resp -> string -> unit =
    fun cmd resp label ->
      let json = response_serializer cmd resp in
      match response_deserializer cmd json with
      | Ok _ -> printf "%s: ok\n" label
      | Error e -> printf "%s: FAIL %s\n" label e
  in
  test Register "tenant-1" "register";
  test Open Local "open/local";
  test Open (Remote "work") "open/remote";
  test Open_on Local "open_on";
  test Test (Match { tenant = "work"; rule_index = 0 }) "test/match";
  test Test (No_match { default_tenant = "default" }) "test/no_match";
  test Get_config {
    listen = [{ host = "127.0.0.1"; port = 9100 }];
    allowed_networks = [];
    tenants = [("default", { browser_cmd = None; label = "Default"; color = "#000"; brand = None })];
    defaults = { unmatched = "default"; cooldown_seconds = 2; browser_launch_timeout = 10 };
  } "get_config";
  test Set_config () "set_config";
  test Get_rules [] "get_rules/empty";
  test Get_rules [{ pattern = ".*\\.example\\.com"; target = "work"; enabled = true }] "get_rules/one";
  test Set_rules () "set_rules";
  test Status { registered_tenants = ["t1"]; uptime_seconds = 42 } "status";
  [%expect {|
    register: ok
    open/local: ok
    open/remote: ok
    open_on: ok
    test/match: ok
    test/no_match: ok
    get_config: ok
    set_config: ok
    get_rules/empty: ok
    get_rules/one: ok
    set_rules: ok
    status: ok
    |}]

let%expect_test "deserialize: invalid json string" =
  begin match parse_json_string "not valid json{" with
  | Error msg -> printf "error=%s\n" msg
  | Ok _ -> print_endline "UNEXPECTED OK"
  end;
  [%expect {|
    error=invalid JSON: Line 1, bytes 0-15:
    Invalid token 'not valid json{'
    |}]
