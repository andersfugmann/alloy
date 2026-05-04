open! Base
open! Stdio

let default_port = 7120
let default_listen = [ "127.0.0.1:7120"; "[::1]:7120" ]

let default_allowed_networks =
  List.map [ "127.0.0.0/8"; "::1/128" ] ~f:(fun s -> Option.value_exn (Cidr.parse s))
