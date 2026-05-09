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

type rules = rule list [@@deriving yojson]

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
  listen : listen_address list; [@default []]
  allowed_networks : Cidr.t list; [@default []]
  tenants : tenants; [@default []]
  defaults : defaults;
}
[@@deriving yojson { strict = false }]

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
  name : string option; [@default None]
}
[@@deriving yojson { strict = false }]

type open_request = {
  url : string;
  title : string option; [@default None]
} [@@deriving yojson]

type open_on_request = {
  target : string;
  url : string;
  title : string option; [@default None]
} [@@deriving yojson]

(* -- Response types *)

type connection_info = { tenant_id : string } [@@deriving yojson]

type history_entry = {
  url : string;
  title : string;
  timestamp : float;
} [@@deriving yojson]

(* -- Lookup request *)

type lookup_request = { query : string } [@@deriving yojson]

type history = history_entry list [@@deriving yojson]

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
  | Connection_info : (unit, connection_info) command
  | Lookup : (lookup_request, history_entry list) command

let parse_json_string s =
  match Yojson.Safe.from_string s with
  | json -> Result.return json
  | exception Yojson.Json_error msg -> Result.failf "invalid JSON: %s" msg

(* -- JSON identity for embedding raw JSON in wire envelopes *)
type json = Yojson.Safe.t [@@deriving yojson]

(* -- Unified wire frame *)

(* TODO: parameterize on the payload type: type frame = 'payload frame { ... payload: 'payload }.
   Then create type aliases e.g. basic_frame = json frame *)

type frame = {
  correlation_id : int; [@key "id"]
  payload : json; [@default `Null]
}
[@@deriving yojson { strict = false }]

type request_payload = {
  command : string;
  params : json; [@default `Null]
}
[@@deriving yojson]


type response_payload = (json, string) Result.t

let response_payload_to_yojson : response_payload -> Yojson.Safe.t = function
  | Ok json -> `List [`String "Ok"; json]
  | Error msg -> `List [`String "Error"; `String msg]

let response_payload_of_yojson : Yojson.Safe.t -> (response_payload, string) Result.t = function
  | `List [`String "Ok"; json] -> Ok (Ok json)
  | `List [`String "Error"; `String msg] -> Ok (Error msg)
  | _ -> Error "response_payload: expected [\"Ok\", ...] or [\"Error\", ...]"

type push =
  | Navigate of string
  | Config_updated of { config : config; registered_tenants : string list }
[@@deriving yojson]

type notification =
  | Broadcast of push
  | Registered of string

let notification_to_yojson = function
  | Broadcast push -> push_to_yojson push
  | Registered tenant -> `List [`String "Registered"; `String tenant]

let notification_of_yojson json =
  match push_of_yojson json with
  | Ok push -> Ok (Broadcast push)
  | Error _ ->
    match json with
    | `List [`String "Registered"; `String tenant] -> Ok (Registered tenant)
    | _ -> Error "notification: expected push or Registered"

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
  | Connection_info -> "connection_info"
  | Lookup -> "lookup"

let command_codec : type req resp. (req, resp) command ->
  ((req -> Yojson.Safe.t) * (Yojson.Safe.t -> (resp, string) Result.t)) *
  ((Yojson.Safe.t -> (req, string) Result.t) * (resp -> Yojson.Safe.t)) = function
  | Register ->
    ((register_request_to_yojson,
      (fun payload -> match payload with
       | `String s -> Ok s
       | _ -> Error "register: expected string")),
     (register_request_of_yojson,
      (fun s -> `String s)))
  | Open ->
    ((open_request_to_yojson, route_result_of_yojson),
     (open_request_of_yojson, route_result_to_yojson))
  | Open_on ->
    ((open_on_request_to_yojson, route_result_of_yojson),
     (open_on_request_of_yojson, route_result_to_yojson))
  | Test ->
    ((open_request_to_yojson, test_result_of_yojson),
     (open_request_of_yojson, test_result_to_yojson))
  | Get_config ->
    (((fun () -> `Null), config_of_yojson),
     ((fun _ -> Ok ()), config_to_yojson))
  | Set_config ->
    ((config_to_yojson, (fun _ -> Ok ())),
     (config_of_yojson, (fun () -> `Null)))
  | Get_rules ->
    (((fun () -> `Null), rules_of_yojson),
     ((fun _ -> Ok ()), rules_to_yojson))
  | Set_rules ->
    ((rules_to_yojson, (fun _ -> Ok ())),
     (rules_of_yojson, (fun () -> `Null)))
  | Status ->
    (((fun () -> `Null), status_info_of_yojson),
     ((fun _ -> Ok ()), status_info_to_yojson))
  | Connection_info ->
    (((fun () -> `Null), connection_info_of_yojson),
     ((fun _ -> Ok ()), connection_info_to_yojson))
  | Lookup ->
    ((lookup_request_to_yojson, history_of_yojson),
     (lookup_request_of_yojson, history_to_yojson))

let request_serializer cmd = fst (fst (command_codec cmd))
let response_deserializer cmd = snd (fst (command_codec cmd))
let request_deserializer cmd = fst (snd (command_codec cmd))
let response_serializer cmd = snd (snd (command_codec cmd))

let make_request_frame : type req resp. (req, resp) command -> req -> int -> frame =
  fun cmd request correlation_id ->
  { correlation_id; payload = request_payload_to_yojson { command = command_name cmd; params = request_serializer cmd request } }

let make_response_frame correlation_id result =
  { correlation_id; payload = response_payload_to_yojson result }

let make_push_frame push =
  { correlation_id = 0; payload = push_to_yojson push }

let make_notification_frame notification =
  { correlation_id = 0; payload = notification_to_yojson notification }

(* -- Frame serialization *)

let serialize_frame frame =
  frame_to_yojson frame |> Yojson.Safe.to_string

let deserialize_frame str =
  let ( let* ) r f = Result.bind r ~f in
  let* json = parse_json_string str in
  frame_of_yojson json

(* -- Inline expect tests *)

let%expect_test "request frame round-trip" =
  let test : type req resp. (req, resp) command -> req -> string -> unit =
    fun cmd params label ->
      let frame = make_request_frame cmd params 42 in
      let json_str = serialize_frame frame in
      match deserialize_frame json_str with
      | Ok frame2 ->
        begin match request_payload_of_yojson frame2.payload with
        | Ok rp ->
          (match String.equal rp.command (command_name cmd) with
           | true -> printf "%s: ok\n" label
           | false -> printf "%s: FAIL command mismatch\n" label)
        | Error e -> printf "%s: FAIL payload: %s\n" label e
        end
      | Error e -> printf "%s: FAIL frame: %s\n" label e
  in
  test Register { brand = Some "Chrome"; name = None } "register";
  test Open { url = "https://x.com"; title = None } "open";
  test Open_on { target = "work"; url = "https://x.com"; title = Some "Example" } "open_on";
  test Test { url = "https://test.com"; title = None } "test";
  test Get_config () "get_config";
  test Get_rules () "get_rules";
  test Set_rules [] "set_rules";
  test Status () "status";
  test Connection_info () "connection_info";
  test Lookup { query = "example" } "lookup";
  [%expect {|
    register: ok
    open: ok
    open_on: ok
    test: ok
    get_config: ok
    get_rules: ok
    set_rules: ok
    status: ok
    connection_info: ok
    lookup: ok
    |}]

let%expect_test "response frame round-trip" =
  let test label result =
    let frame = make_response_frame 1 result in
    let json_str = serialize_frame frame in
    match deserialize_frame json_str with
    | Ok frame2 ->
      begin match response_payload_of_yojson frame2.payload with
      | Ok (Ok _) -> printf "%s: success\n" label
      | Ok (Error msg) -> printf "%s: failure: %s\n" label msg
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
      begin match push_of_yojson frame2.payload with
      | Ok _ -> printf "%s: ok\n" label
      | Error e -> printf "%s: FAIL: %s\n" label e
      end
    | Error e -> printf "%s: FAIL: %s\n" label e
  in
  test "navigate" (Navigate "https://x.com");
  test "config_updated" (Config_updated {
    config = {
      listen = []; allowed_networks = []; tenants = [];
      defaults = { unmatched = "default"; cooldown_seconds = 2; browser_launch_timeout = 10 }
    };
    registered_tenants = ["t1"]
  });
  [%expect {|
    navigate: ok
    config_updated: ok
    |}]

let%expect_test "notification frame round-trip" =
  let test label notification =
    let frame = make_notification_frame notification in
    let json_str = serialize_frame frame in
    match deserialize_frame json_str with
    | Ok frame2 ->
      begin match notification_of_yojson frame2.payload with
      | Ok _ -> printf "%s: ok\n" label
      | Error e -> printf "%s: FAIL: %s\n" label e
      end
    | Error e -> printf "%s: FAIL: %s\n" label e
  in
  test "broadcast/navigate" (Broadcast (Navigate "https://x.com"));
  test "registered" (Registered "test");
  [%expect {|
    broadcast/navigate: ok
    registered: ok
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
  test Connection_info { tenant_id = "zaphod-chromium" } "connection_info";
  test Lookup [{ url = "https://x.com"; title = "X"; timestamp = 1234567890.0 }] "lookup";
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
    connection_info: ok
    lookup: ok
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

let%expect_test "config backward compat: old config with rules field" =
  let old_config = {|{
    "listen": [{"host": "127.0.0.1", "port": 7120}],
    "allowed_networks": ["127.0.0.0/8"],
    "tenants": {"work": {"label": "Work", "color": "#FF0000"}},
    "defaults": {"unmatched": "local", "cooldown_seconds": 5, "browser_launch_timeout": 10},
    "rules": [{"pattern": ".*example.*", "target": "work", "enabled": true}]
  }|} in
  let json = Yojson.Safe.from_string old_config in
  begin match config_of_yojson json with
  | Ok _config -> printf "config with rules: ok\n"
  | Error msg -> printf "config with rules: FAIL %s\n" msg
  end;
  (* Verify rules can be extracted from the raw JSON *)
  let rules_json = Yojson.Safe.Util.member "rules" json in
  begin match rules_of_yojson rules_json with
  | Ok rules -> printf "rules: ok (%d rules)\n" (List.length rules)
  | Error msg -> printf "rules: FAIL %s\n" msg
  end;
  (* Old config without listen/allowed_networks *)
  let minimal_config = {|{
    "tenants": {"work": {"label": "Work", "color": "#FF0000"}},
    "defaults": {"unmatched": "local", "cooldown_seconds": 5, "browser_launch_timeout": 10},
    "rules": [{"pattern": ".*example.*", "target": "work", "enabled": true}]
  }|} in
  begin match config_of_yojson (Yojson.Safe.from_string minimal_config) with
  | Ok config ->
    printf "minimal config: ok (listen=%d, networks=%d)\n"
      (List.length config.listen) (List.length config.allowed_networks)
  | Error msg -> printf "minimal config: FAIL %s\n" msg
  end;
  [%expect {|
    config with rules: ok
    rules: ok (1 rules)
    minimal config: ok (listen=0, networks=0)
    |}]
