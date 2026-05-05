open! Base
open! Stdio

(* -- Network defaults *)

let default_port = 7120
let default_host = "127.0.0.1"

let default_listen : Protocol.listen_address list =
  [ { host = "127.0.0.1"; port = default_port };
    { host = "::1"; port = default_port } ]

let default_allowed_networks =
  List.map [ "127.0.0.0/8"; "::1/128" ] ~f:(fun s -> Option.value_exn (Cidr.parse s))

(* -- Buffer and queue sizes *)

let max_read_buffer = 1024 * 1024
let push_queue_capacity = 16
let coordinator_inbox_capacity = 64
let bridge_stream_capacity = 64
let tcp_listen_backlog = 128

(* -- Timing *)

let default_cooldown_seconds = 5
let default_browser_launch_timeout = 10
let reconnect_delay_ms = 2000

(* -- UI *)

let popup_width = 420
let popup_height = 300

(* -- Styling *)

let default_tenant_color = "#808080"
