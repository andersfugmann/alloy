open !Base
open !Stdio

(* -- Constants *)

let default_port = 7120

(* -- Address parsing *)

type address = { host : string; port : int }

let parse_address (s : string) : address =
  let s = String.strip s in
  (* Handle IPv6 [host]:port *)
  match String.lsplit2 s ~on:']' with
  | Some (bracketed, after_bracket) ->
    let host = String.lstrip ~drop:(Char.equal '[') bracketed in
    let port =
      match String.lsplit2 after_bracket ~on:':' with
      | Some (_, p) -> Int.of_string_opt p |> Option.value ~default:default_port
      | None -> default_port
    in
    { host; port }
  | None ->
    begin match String.rsplit2 s ~on:':' with
    | Some (host, port_s) ->
      let port = Int.of_string_opt port_s |> Option.value ~default:default_port in
      { host; port }
    | None -> { host = s; port = default_port }
    end

let default_allowed_networks =
  List.filter_map [ "127.0.0.0/8"; "::1/128" ] ~f:Cidr.parse

let internal_url_prefixes =
  [ "chrome://"; "chrome-extension://"; "about:"; "edge://"; "brave://";
    "chrome-search://"; "devtools://" ]

let is_internal_url url =
  List.exists internal_url_prefixes ~f:(fun prefix -> String.is_prefix url ~prefix)

(* -- Core data types *)

type tenant_id = string [@@deriving yojson]
type url = string [@@deriving yojson]

type rule = {
  pattern : string;
  target : tenant_id;
  enabled : bool;
}
[@@deriving yojson]

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

(* Serialize tenant map as a JSON object keyed by tenant ID *)
let tenants_to_yojson (lst : (string * tenant_config) list) : Yojson.Safe.t =
  `Assoc
    (List.map lst ~f:(fun (k, v) -> (k, tenant_config_to_yojson v)))

let tenants_of_yojson (json : Yojson.Safe.t) :
    ((string * tenant_config) list, string) Result.t =
  match json with
  | `Assoc pairs ->
    List.fold_result pairs ~init:[] ~f:(fun acc (k, v) ->
        tenant_config_of_yojson v |> Result.map ~f:(fun tc -> (k, tc) :: acc))
    |> Result.map ~f:List.rev
  | _ -> Error "tenants: expected JSON object"

let default_listen = [ "127.0.0.1:7120"; "[::1]:7120" ]

type config = {
  listen : string list;
  allowed_networks : Cidr.t list;
  tenants : (string * tenant_config) list;
      [@to_yojson tenants_to_yojson] [@of_yojson tenants_of_yojson]
  rules : rule list;
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

(* -- GADT command type *)

type _ command =
  | Register : string option -> string command
  | Open : url -> route_result command
  | Open_on : tenant_id * url -> route_result command
  | Test : url -> test_result command
  | Get_config : config command
  | Set_config : config -> unit command
  | Add_rule : rule -> unit command
  | Update_rule : int * rule -> unit command
  | Delete_rule : int -> unit command
  | Status : status_info command

(* -- Existential wrappers *)

type packed_command = Command : 'a command -> packed_command

(* -- Helpers *)

let ( let* ) r f = Result.bind r ~f

let parse_json_string (s : string) : (Yojson.Safe.t, string) Result.t =
  match Yojson.Safe.from_string s with
  | json -> Ok json
  | exception Yojson.Json_error msg -> Error (Printf.sprintf "invalid JSON: %s" msg)

(* -- JSON wire types *)

module Wire = struct
  type command =
    | Register of { brand : string option [@default None]; address : string option [@default None]; name : string option [@default None] }
    | Open of { url : string }
    | Open_on of { target : string; url : string }
    | Test of { url : string }
    | Get_config
    | Set_config of { config : config }
    | Add_rule of { rule : rule }
    | Update_rule of { index : int; rule : rule }
    | Delete_rule of { index : int }
    | Status
  [@@deriving yojson]

  type response =
    | Ok_unit
    | Ok_registered of { tenant_id : string }
    | Ok_route of route_result
    | Ok_test of test_result
    | Ok_config of config
    | Ok_status of status_info
    | Err of { message : string }
  [@@deriving yojson]

  type push =
    | Navigate of { url : string }
    | Registered of { tenant_id : string }
    | Config_updated of { config : config; registered_tenants : string list }
  [@@deriving yojson]

  type request = {
    id : int;
    command : command;
    tenant : string option; [@default None]
  }
  [@@deriving yojson]

  type server_message =
    | Response of { id : int; response : response }
    | Push of { id : int; push : push }
  [@@deriving yojson]
end

(* -- Wire type conversions *)

let command_to_wire : type a. a command -> Wire.command = function
  | Register brand -> Register { brand; address = None; name = None }
  | Open url -> Open { url }
  | Open_on (target, url) -> Open_on { target; url }
  | Test url -> Test { url }
  | Get_config -> Get_config
  | Set_config cfg -> Set_config { config = cfg }
  | Add_rule r -> Add_rule { rule = r }
  | Update_rule (idx, r) -> Update_rule { index = idx; rule = r }
  | Delete_rule idx -> Delete_rule { index = idx }
  | Status -> Status

let command_of_wire (w : Wire.command) : packed_command =
  match w with
  | Register { brand; _ } -> Command (Register brand)
  | Open { url } -> Command (Open url)
  | Open_on { target; url } -> Command (Open_on (target, url))
  | Test { url } -> Command (Test url)
  | Get_config -> Command Get_config
  | Set_config { config } -> Command (Set_config config)
  | Add_rule { rule } -> Command (Add_rule rule)
  | Update_rule { index; rule } -> Command (Update_rule (index, rule))
  | Delete_rule { index } -> Command (Delete_rule index)
  | Status -> Command Status

let response_to_wire : type a. a command -> (a, string) Result.t -> Wire.response =
 fun cmd resp ->
  match resp with
  | Error msg -> Err { message = msg }
  | Ok value ->
    (match cmd with
     | Register _ -> Ok_registered { tenant_id = value }
     | Open _ -> Ok_route value
     | Open_on _ -> Ok_route value
     | Test _ -> Ok_test value
     | Get_config -> Ok_config value
     | Set_config _ -> Ok_unit
     | Add_rule _ -> Ok_unit
     | Update_rule _ -> Ok_unit
     | Delete_rule _ -> Ok_unit
     | Status -> Ok_status value)

let response_of_wire : type a. a command -> Wire.response -> (a, string) Result.t =
 fun cmd resp ->
  match resp with
  | Err { message } -> Error message
  | Ok_registered { tenant_id } ->
    (match cmd with
     | Register _ -> Ok tenant_id
     | _ -> Error "unexpected Ok_registered")
  | Ok_route r ->
    begin match cmd with
    | Open _ -> Ok r
    | Open_on _ -> Ok r
    | _ -> Error "unexpected Ok_route"
    end
  | Ok_test t ->
    (match cmd with
     | Test _ -> Ok t
     | _ -> Error "unexpected Ok_test")
  | Ok_config c ->
    (match cmd with
     | Get_config -> Ok c
     | _ -> Error "unexpected Ok_config")
  | Ok_status s ->
    (match cmd with
     | Status -> Ok s
     | _ -> Error "unexpected Ok_status")
  | Ok_unit ->
    begin match cmd with
    | Set_config _ -> Ok ()
    | Add_rule _ -> Ok ()
    | Update_rule _ -> Ok ()
    | Delete_rule _ -> Ok ()
    | _ -> Error "unexpected Ok_unit"
    end

(* -- JSON serialization helpers *)

let serialize_command_json : type a. a command -> Yojson.Safe.t =
 fun cmd -> command_to_wire cmd |> Wire.command_to_yojson

let deserialize_command_json (json : Yojson.Safe.t) :
    (packed_command, string) Result.t =
  let* wire = Wire.command_of_yojson json in
  Ok (command_of_wire wire)

let wire_command_name : Wire.command -> string = function
  | Register _ -> "Register"
  | Open _ -> "Open"
  | Open_on _ -> "Open_on"
  | Test _ -> "Test"
  | Get_config -> "Get_config"
  | Set_config _ -> "Set_config"
  | Add_rule _ -> "Add_rule"
  | Update_rule _ -> "Update_rule"
  | Delete_rule _ -> "Delete_rule"
  | Status -> "Status"

let serialize_server_message (msg : Wire.server_message) : string =
  Wire.server_message_to_yojson msg |> Yojson.Safe.to_string

let deserialize_server_message (s : string) :
    (Wire.server_message, string) Result.t =
  let* json = parse_json_string s in
  Wire.server_message_of_yojson json

let serialize_request (req : Wire.request) : string =
  Wire.request_to_yojson req |> Yojson.Safe.to_string

let deserialize_request (s : string) : (Wire.request, string) Result.t =
  let* json = parse_json_string s in
  Wire.request_of_yojson json

(* -- Inline expect tests *)

let%expect_test "sample data" =
  let _rule =
    { pattern = ".*[.]example[.]com"; target = "work"; enabled = true }
  in
  let _tenant_cfg =
    { browser_cmd = Some "chromium"; label = "Work"; color = "#0000ff"; brand = None }
  in
  let _defaults =
    { unmatched = "personal"; cooldown_seconds = 5; browser_launch_timeout = 10 }
  in
  [%expect {||}]

(* -- JSON: commands *)

let%expect_test "json: round-trip register" =
  let json = serialize_command_json (Register (Some "Google Chrome")) in
  print_endline (Yojson.Safe.to_string json);
  (match deserialize_command_json json with
   | Ok (Command (Register (Some brand))) -> printf "brand=%s\n" brand
   | _ -> print_endline "FAIL");
  [%expect {|
    ["Register",{"brand":"Google Chrome"}]
    brand=Google Chrome
    |}]

let%expect_test "json: round-trip open" =
  let json = serialize_command_json (Open "https://example.com") in
  print_endline (Yojson.Safe.to_string json);
  (match deserialize_command_json json with
   | Ok (Command (Open url)) -> printf "url=%s\n" url
   | _ -> print_endline "FAIL");
  [%expect {|
    ["Open",{"url":"https://example.com"}]
    url=https://example.com
    |}]

let%expect_test "json: round-trip open_on" =
  let json = serialize_command_json (Open_on ("work", "https://example.com")) in
  (match deserialize_command_json json with
   | Ok (Command (Open_on (target, url))) ->
     printf "target=%s url=%s\n" target url
   | _ -> print_endline "FAIL");
  [%expect {| target=work url=https://example.com |}]

let%expect_test "json: round-trip test" =
  let json = serialize_command_json (Test "https://example.com") in
  (match deserialize_command_json json with
   | Ok (Command (Test url)) -> printf "url=%s\n" url
   | _ -> print_endline "FAIL");
  [%expect {| url=https://example.com |}]

let%expect_test "json: round-trip set_config" =
  let cfg = {
    listen = [ "127.0.0.1:7120"; "[::1]:7120" ];
    allowed_networks = default_allowed_networks;
    tenants =
      [ ( "work",
          { browser_cmd = Some "chromium --profile-directory=Work";
            label = "Work"; color = "#0000ff"; brand = Some "Google Chrome" } ) ];
    rules = [ { pattern = ".*[.]example[.]com"; target = "work"; enabled = true } ];
    defaults = { unmatched = "personal"; cooldown_seconds = 5; browser_launch_timeout = 10 };
  } in
  let json = serialize_command_json (Set_config cfg) in
  (match deserialize_command_json json with
   | Ok (Command (Set_config cfg)) ->
     printf "listen=%s\n" (String.concat ~sep:"," cfg.listen)
   | _ -> print_endline "FAIL");
  [%expect {| listen=127.0.0.1:7120,[::1]:7120 |}]

let%expect_test "json: round-trip add_rule" =
  let rule = { pattern = ".*[.]example[.]com"; target = "work"; enabled = true } in
  let json = serialize_command_json (Add_rule rule) in
  (match deserialize_command_json json with
   | Ok (Command (Add_rule r)) -> printf "pattern=%s\n" r.pattern
   | _ -> print_endline "FAIL");
  [%expect {| pattern=.*[.]example[.]com |}]

let%expect_test "json: round-trip update_rule" =
  let rule = { pattern = ".*[.]example[.]com"; target = "work"; enabled = true } in
  let json = serialize_command_json (Update_rule (5, rule)) in
  (match deserialize_command_json json with
   | Ok (Command (Update_rule (idx, r))) ->
     printf "idx=%d pattern=%s\n" idx r.pattern
   | _ -> print_endline "FAIL");
  [%expect {| idx=5 pattern=.*[.]example[.]com |}]

let%expect_test "json: round-trip delete_rule" =
  let json = serialize_command_json (Delete_rule 7) in
  (match deserialize_command_json json with
   | Ok (Command (Delete_rule idx)) -> printf "idx=%d\n" idx
   | _ -> print_endline "FAIL");
  [%expect {| idx=7 |}]

(* -- JSON: request *)

let%expect_test "json: request with tenant" =
  let req : Wire.request = { id = 1; command = Register { brand = Some "Edge"; address = None; name = None }; tenant = Some "mypc" } in
  let s = serialize_request req in
  print_endline s;
  (match deserialize_request s with
   | Ok r -> printf "id=%d tenant=%s\n" r.id (Option.value r.tenant ~default:"(none)")
   | Error msg -> printf "FAIL: %s\n" msg);
  [%expect {|
    {"id":1,"command":["Register",{"brand":"Edge"}],"tenant":"mypc"}
    id=1 tenant=mypc
    |}]

let%expect_test "json: request without tenant" =
  let req : Wire.request = { id = 2; command = Get_config; tenant = None } in
  let s = serialize_request req in
  print_endline s;
  (match deserialize_request s with
   | Ok r -> printf "id=%d tenant=%s\n" r.id (Option.value r.tenant ~default:"(none)")
   | Error msg -> printf "FAIL: %s\n" msg);
  [%expect {|
    {"id":2,"command":["Get_config"]}
    id=2 tenant=(none)
    |}]

(* -- JSON: server_message *)

let%expect_test "json: server_message response" =
  let msg = Wire.Response { id = 1; response = Ok_registered { tenant_id = "mypc" } } in
  let s = serialize_server_message msg in
  print_endline s;
  (match deserialize_server_message s with
   | Ok (Response { id; response = Ok_registered { tenant_id } }) ->
     printf "id=%d tenant_id=%s\n" id tenant_id
   | _ -> print_endline "FAIL");
  [%expect {|
    ["Response",{"id":1,"response":["Ok_registered",{"tenant_id":"mypc"}]}]
    id=1 tenant_id=mypc
    |}]

let%expect_test "json: server_message push" =
  let msg = Wire.Push { id = 0; push = Navigate { url = "https://example.com" } } in
  let s = serialize_server_message msg in
  print_endline s;
  (match deserialize_server_message s with
   | Ok (Push { id; push = Navigate { url } }) ->
     printf "id=%d url=%s\n" id url
   | _ -> print_endline "FAIL");
  [%expect {|
    ["Push",{"id":0,"push":["Navigate",{"url":"https://example.com"}]}]
    id=0 url=https://example.com
    |}]

let%expect_test "json: server_message push config_updated" =
  let cfg : config = {
    listen = ["127.0.0.1:7120"];
    allowed_networks = List.filter_map ["127.0.0.0/8"] ~f:Cidr.parse;
    tenants = [("work", { browser_cmd = None; label = "Work"; color = "#ff0000"; brand = None })];
    rules = [];
    defaults = { unmatched = "local"; cooldown_seconds = 5; browser_launch_timeout = 10 };
  } in
  let msg = Wire.Push { id = 0; push = Config_updated { config = cfg; registered_tenants = ["work"] } } in
  let s = serialize_server_message msg in
  (match deserialize_server_message s with
   | Ok (Push { id; push = Config_updated { config = c; registered_tenants = r } }) ->
     printf "id=%d tenants=%d registered=%d\n" id (List.length c.tenants) (List.length r)
   | _ -> print_endline "FAIL");
  [%expect {| id=0 tenants=1 registered=1 |}]

let%expect_test "json: server_message error response" =
  let msg = Wire.Response { id = 5; response = Err { message = "not found" } } in
  let s = serialize_server_message msg in
  (match deserialize_server_message s with
   | Ok (Response { id; response = Err { message } }) ->
     printf "id=%d err=%s\n" id message
   | _ -> print_endline "FAIL");
  [%expect {| id=5 err=not found |}]

let%expect_test "json: response config round-trip" =
  let cfg = {
    listen = [ "127.0.0.1:7120"; "[::1]:7120" ];
    allowed_networks = default_allowed_networks;
    tenants =
      [ ( "work",
          { browser_cmd = Some "chromium --profile-directory=Work";
            label = "Work"; color = "#0000ff"; brand = Some "Google Chrome" } ) ];
    rules = [ { pattern = ".*[.]example[.]com"; target = "work"; enabled = true } ];
    defaults = { unmatched = "personal"; cooldown_seconds = 5; browser_launch_timeout = 10 };
  } in
  let wire_resp = response_to_wire Get_config (Ok cfg) in
  let msg = Wire.Response { id = 42; response = wire_resp } in
  let s = serialize_server_message msg in
  (match deserialize_server_message s with
   | Ok (Response { id; response = Ok_config cfg }) ->
     printf "id=%d listen=%s rules=%d\n" id
       (String.concat ~sep:"," cfg.listen) (List.length cfg.rules)
   | _ -> print_endline "FAIL");
  [%expect {| id=42 listen=127.0.0.1:7120,[::1]:7120 rules=1 |}]

(* -- Error handling *)

let%expect_test "deserialize: invalid json string" =
  (match parse_json_string "not valid json{" with
   | Error msg -> printf "error=%s\n" msg
   | Ok _ -> print_endline "UNEXPECTED OK");
  [%expect {|
    error=invalid JSON: Line 1, bytes 0-15:
    Invalid token 'not valid json{'
    |}]
