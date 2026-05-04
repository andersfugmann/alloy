open! Base
open! Stdio

type t = Ipaddr.Prefix.t

let of_yojson = function
  | `String s ->
    (match Ipaddr.Prefix.of_string s with
     | Ok prefix -> Ok prefix
     | Error _ ->
       match Ipaddr.of_string s with
       | Ok ip -> Ok (Ipaddr.Prefix.of_addr ip)
       | Error (`Msg msg) -> Error msg)
  | _ -> Error "expected string for CIDR"

let to_yojson prefix =
  `String (Ipaddr.Prefix.to_string prefix)

let parse s =
  match of_yojson (`String s) with
  | Ok v -> Some v
  | Error _ -> None

let ip_allowed ~allowed_networks ip_str =
  match Ipaddr.of_string ip_str with
  | Error _ -> false
  | Ok ip -> List.exists allowed_networks ~f:(fun prefix -> Ipaddr.Prefix.mem ip prefix)
