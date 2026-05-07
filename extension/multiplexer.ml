open! Base
open! Stdio

let is_register_frame (frame : Protocol.frame) : bool =
  match Protocol.parse_request_payload frame with
  | Ok { command; _ } -> String.equal command "register"
  | Error _ -> false

let assign_tenant_id (ports : Chrome_api.port Map.M(String).t) (desired : string) (counter : int) : string =
  let rec find_unique name n =
    match Map.mem ports name with
    | false -> name
    | true -> find_unique (Printf.sprintf "%s_%d" desired (n + 1)) (n + 1)
  in
  find_unique desired (counter + 1)
