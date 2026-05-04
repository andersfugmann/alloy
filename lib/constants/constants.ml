open! Base
open! Stdio

let default_port = 7120
let default_host = "127.0.0.1"

let default_listen : Protocol.listen_address list =
  [ { host = "127.0.0.1"; port = default_port };
    { host = "::1"; port = default_port } ]

let default_allowed_networks =
  List.map [ "127.0.0.0/8"; "::1/128" ] ~f:(fun s -> Option.value_exn (Cidr.parse s))
