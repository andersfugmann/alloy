open! Base
open! Stdio

type request = {
  msg : string;
  address : Protocol.listen_address; [@key "payload"]
  debug : bool;
}
[@@deriving yojson]

type connected = {
  status : string;
  hostname : string;
}
[@@deriving yojson]

type error = {
  status : string;
  error : string;
}
[@@deriving yojson]

type response_payload =
  | Connected of connected
  | Bridge_error of error
[@@deriving yojson]

type response = {
  msg : string;
  result : response_payload;
}
[@@deriving yojson]

let make_request ~debug addr =
  { msg = "connect"; address = addr; debug }

let make_connected hostname =
  { msg = "connected"; result = Connected { status = "connected"; hostname } }

let make_error error =
  { msg = "connected"; result = Bridge_error { status = "error"; error } }

let parse_request json =
  match request_of_yojson json with
  | Ok req ->
    begin match String.equal req.msg "connect" with
    | true -> Ok (req.address, req.debug)
    | false -> Error (Printf.sprintf "expected msg=connect, got %s" req.msg)
    end
  | Error e -> Error e

let parse_response json =
  match response_of_yojson json with
  | Ok resp ->
    begin match resp.result with
    | Connected c -> Ok c
    | Bridge_error e -> Error e.error
    end
  | Error e -> Error e
