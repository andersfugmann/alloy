(** Multi-tenant URL routing protocol types and serialization. *)

(** {1 Constants} *)

val default_port : int

(** {1 Address parsing} *)

type address = { host : string; port : int }

val parse_address : string -> address

(** {1 Network defaults} *)

val default_allowed_networks : Cidr.t list

val is_internal_url : string -> bool

(** {1 Core data types} *)

type tenant_id = string [@@deriving yojson]
type url = string [@@deriving yojson]

type rule = {
  pattern : string;
  target : tenant_id;
  enabled : bool;
}
[@@deriving yojson]

type tenant_config = {
  browser_cmd : string option;
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

val default_listen : string list

type config = {
  listen : string list;
  allowed_networks : Cidr.t list;
  tenants : (string * tenant_config) list;
  rules : rule list;
  defaults : defaults;
}
[@@deriving yojson]

type status_info = {
  registered_tenants : tenant_id list;
  uptime_seconds : int;
}
[@@deriving yojson]

(** {1 Response payload types} *)

type route_result =
  | Local
  | Remote of tenant_id
[@@deriving yojson]

type test_result =
  | Match of { tenant : tenant_id; rule_index : int }
  | No_match of { default_tenant : tenant_id }
[@@deriving yojson]

(** {1 GADT command type} *)

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

(** {1 Existential wrappers} *)

type packed_command = Command : 'a command -> packed_command

(** {1 JSON wire types} *)

module Wire : sig
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

(** {1 Wire type conversions} *)

val command_to_wire : 'a command -> Wire.command
val command_of_wire : Wire.command -> packed_command
val response_to_wire : 'a command -> ('a, string) Result.t -> Wire.response
val response_of_wire : 'a command -> Wire.response -> ('a, string) Result.t

(** {1 JSON serialization — commands} *)

val serialize_command_json : 'a command -> Yojson.Safe.t
val deserialize_command_json : Yojson.Safe.t -> (packed_command, string) Result.t

(** {1 JSON serialization — pushes} *)

(** {1 Logging helpers} *)

val name_of_command : Wire.command -> string

(** {1 JSON serialization — server messages} *)

val serialize_server_message : Wire.server_message -> string
val deserialize_server_message : string -> (Wire.server_message, string) Result.t

(** {1 JSON serialization — requests} *)

val serialize_request : Wire.request -> string
val deserialize_request : string -> (Wire.request, string) Result.t

(** {1 JSON helpers} *)

val parse_json_string : string -> (Yojson.Safe.t, string) Result.t
