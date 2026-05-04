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

let%test "round-trip IPv4 CIDR" =
  let t = Option.value_exn (parse "192.168.1.0/24") in
  to_yojson t |> of_yojson |> Result.ok |> Option.value_exn |> Ipaddr.Prefix.compare t |> Int.equal 0

let%test "round-trip IPv6 CIDR" =
  let t = Option.value_exn (parse "fd00::/8") in
  to_yojson t |> of_yojson |> Result.ok |> Option.value_exn |> Ipaddr.Prefix.compare t |> Int.equal 0

let%test "round-trip bare IPv4" =
  let t = Option.value_exn (parse "127.0.0.1") in
  to_yojson t |> of_yojson |> Result.ok |> Option.value_exn |> Ipaddr.Prefix.compare t |> Int.equal 0
