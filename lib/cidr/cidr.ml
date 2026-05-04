open! Base
open! Stdio

type t = Ipaddr.Prefix.t

let parse s =
  match Ipaddr.Prefix.of_string s with
  | Ok prefix -> Some prefix
  | Error _ ->
    (* Try as a bare IP address — use /32 or /128 *)
    match Ipaddr.of_string s with
    | Ok ip -> Some (Ipaddr.Prefix.of_addr ip)
    | Error _ -> None

let ip_matches ip_str prefix =
  match Ipaddr.of_string ip_str with
  | Ok ip -> Ipaddr.Prefix.mem ip prefix
  | Error _ -> false

let ip_allowed ~allowed_networks ip =
  List.exists allowed_networks ~f:(fun prefix -> ip_matches ip prefix)
