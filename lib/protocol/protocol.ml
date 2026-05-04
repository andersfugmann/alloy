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
  List.map [ "127.0.0.0/8"; "::1/128" ] ~f:(fun s -> Option.value_exn (Cidr.parse s))

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

let default_listen = [ "127.0.0.1:7120"; "[::1]:7120" ]

type config = {
  listen : string list;
  allowed_networks : Cidr.t list;
  tenants : tenants;
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
  | json -> Result.return json
  | exception Yojson.Json_error msg -> Result.failf "invalid JSON: %s" msg

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

let name_of_resp = function
  | Wire.Ok_unit -> "Ok_unit"
  | Wire.Ok_registered _ -> "Ok_registered"
  | Wire.Ok_route _ -> "Ok_route"
  | Wire.Ok_test _ -> "Ok_test"
  | Wire.Ok_config _ -> "Ok_config"
  | Wire.Ok_status _ -> "Ok_status"
  | Wire.Err _ -> "Err"

let response_of_wire : type a. a command -> Wire.response -> (a, string) Result.t =
  fun cmd resp ->
  let open Result in
  match resp, cmd with
  | Err { message }, _ -> fail message
  | Ok_registered { tenant_id }, Register _ -> return tenant_id
  | Ok_route r, Open _ -> return r
  | Ok_route r, Open_on _ -> return r
  | Ok_test t, Test _  -> return t
  | Ok_config c, Get_config -> return c
  | Ok_status s, Status -> return s
  | Ok_unit, Set_config _ -> return ()
  | Ok_unit, Add_rule _ -> return ()
  | Ok_unit, Update_rule _ -> return ()
  | Ok_unit, Delete_rule _ -> return ()
  | resp, _ -> failf "unexpected %s" (name_of_resp resp)

(* -- JSON serialization helpers *)

let serialize_command_json : type a. a command -> Yojson.Safe.t =
 fun cmd -> command_to_wire cmd |> Wire.command_to_yojson

let deserialize_command_json (json : Yojson.Safe.t) :
    (packed_command, string) Result.t =
  let* wire = Wire.command_of_yojson json in
  Result.return (command_of_wire wire)

let name_of_command : Wire.command -> string = function
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

let%expect_test "GADT command round-trip" =
  let test : type a. a command -> string -> unit =
    fun cmd label ->
      match serialize_command_json cmd |> deserialize_command_json with
      | Ok (Command _) -> printf "%s: ok\n" label
      | Error e -> printf "%s: FAIL %s\n" label e
  in
  test (Register (Some "Chrome")) "register";
  test (Open "https://x.com") "open";
  test (Open_on ("work", "https://x.com")) "open_on";
  test Get_config "get_config";
  test Status "status";
  test (Delete_rule 3) "delete_rule";
  [%expect {|
    register: ok
    open: ok
    open_on: ok
    get_config: ok
    status: ok
    delete_rule: ok
    |}]

let%expect_test "deserialize: invalid json string" =
  (match parse_json_string "not valid json{" with
   | Error msg -> printf "error=%s\n" msg
   | Ok _ -> print_endline "UNEXPECTED OK");
  [%expect {|
    error=invalid JSON: Line 1, bytes 0-15:
    Invalid token 'not valid json{'
    |}]
