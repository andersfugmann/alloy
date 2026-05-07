open! Base
open! Stdio

let is_register_frame (frame : Protocol.frame) : bool =
  match Protocol.parse_request_payload frame with
  | Ok { command; _ } -> String.equal command "register"
  | Error _ -> false
