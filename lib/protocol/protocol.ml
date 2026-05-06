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

let json_to_string (json : Yojson.Safe.t) : string =
  Yojson.Safe.to_string json

(* -- JSON identity for embedding raw JSON in wire envelopes *)

type json = Yojson.Safe.t
let json_to_yojson (x : json) : Yojson.Safe.t = x
let json_of_yojson (x : Yojson.Safe.t) : (json, string) Result.t = Ok x

(* -- Unified wire frame *)

type frame = {
  id : int;
  tenant : string option; [@default None]
  payload : json; [@default `Null]
}
[@@deriving yojson]

type request_payload = {
  command : string;
  params : json; [@default `Null]
}
[@@deriving yojson]

type response_payload =
  | Success of json
  | Failure of string
[@@deriving yojson]

(* -- Push messages *)

type push =
  | Navigate of { url : string }
  | Registered of { tenant_id : string }
  | Config_updated of { config : config; registered_tenants : string list }
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

let request_deserializer : type req resp. (req, resp) command -> (Yojson.Safe.t -> (req, string) Result.t) = function
  | Register -> register_request_of_yojson
  | Open -> open_request_of_yojson
  | Open_on -> open_on_request_of_yojson
  | Test -> open_request_of_yojson
  | Get_config -> (fun _ -> Ok ())
  | Set_config -> config_of_yojson
  | Get_rules -> (fun _ -> Ok ())
  | Set_rules -> rules_of_yojson
  | Status -> (fun _ -> Ok ())

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

(* -- Frame construction *)

let make_request_frame : type req resp. (req, resp) command -> req -> int -> string option -> frame =
  fun cmd request id tenant ->
  { id; tenant; payload = request_payload_to_yojson { command = command_name cmd; params = request_serializer cmd request } }

let make_request_frame_raw ~command ~params ~id ~tenant =
  { id; tenant; payload = request_payload_to_yojson { command; params } }

let make_response_frame id ?tenant result =
  let rp =
    match result with
    | Ok json -> Success json
    | Error msg -> Failure msg
  in
  { id; tenant; payload = response_payload_to_yojson rp }

let make_push_frame push =
  { id = 0; tenant = None; payload = push_to_yojson push }

(* -- Frame serialization *)

let serialize_frame frame =
  frame_to_yojson frame |> Yojson.Safe.to_string

let deserialize_frame str =
  let* json = parse_json_string str in
  frame_of_yojson json

let parse_request_payload frame =
  request_payload_of_yojson frame.payload

let parse_response_payload frame =
  response_payload_of_yojson frame.payload

let parse_push_payload frame =
  push_of_yojson frame.payload

(* -- Inline expect tests *)

let%expect_test "request frame round-trip" =
  let test : type req resp. (req, resp) command -> req -> string -> unit =
    fun cmd params label ->
      let frame = make_request_frame cmd params 42 (Some "test-tenant") in
      let json_str = serialize_frame frame in
      match deserialize_frame json_str with
      | Ok frame2 ->
        begin match parse_request_payload frame2 with
        | Ok rp ->
          (match String.equal rp.command (command_name cmd) with
           | true -> printf "%s: ok\n" label
           | false -> printf "%s: FAIL command mismatch\n" label)
        | Error e -> printf "%s: FAIL payload: %s\n" label e
        end
      | Error e -> printf "%s: FAIL frame: %s\n" label e
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

let%expect_test "response frame round-trip" =
  let test label result =
    let frame = make_response_frame 1 result in
    let json_str = serialize_frame frame in
    match deserialize_frame json_str with
    | Ok frame2 ->
      begin match parse_response_payload frame2 with
      | Ok (Success _) -> printf "%s: success\n" label
      | Ok (Failure msg) -> printf "%s: failure: %s\n" label msg
      | Error e -> printf "%s: FAIL: %s\n" label e
      end
    | Error e -> printf "%s: FAIL: %s\n" label e
  in
  test "ok" (Ok (`String "hello"));
  test "error" (Error "something went wrong");
  [%expect {|
    ok: success
    error: failure: something went wrong
    |}]

let%expect_test "push frame round-trip" =
  let test label push =
    let frame = make_push_frame push in
    let json_str = serialize_frame frame in
    match deserialize_frame json_str with
    | Ok frame2 ->
      begin match parse_push_payload frame2 with
      | Ok _ -> printf "%s: ok\n" label
      | Error e -> printf "%s: FAIL: %s\n" label e
      end
    | Error e -> printf "%s: FAIL: %s\n" label e
  in
  test "navigate" (Navigate { url = "https://x.com" });
  test "registered" (Registered { tenant_id = "test" });
  test "config_updated" (Config_updated {
    config = {
      listen = []; allowed_networks = []; tenants = [];
      defaults = { unmatched = "default"; cooldown_seconds = 2; browser_launch_timeout = 10 }
    };
    registered_tenants = ["t1"]
  });
  [%expect {|
    navigate: ok
    registered: ok
    config_updated: ok
    |}]

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
