open! Base
open! Stdio

let ( let* ) = Lwt.bind

type daemon = {
  pid : int;
  port : int;
  config_dir : string;
}

let test_config ~port =
  Printf.sprintf {|{
  "listen": [{"host": "127.0.0.1", "port": %d}],
  "allowed_networks": ["127.0.0.0/8"],
  "tenants": {
    "test-tenant": {
      "label": "Test Tenant",
      "color": "#00ff00"
    }
  },
  "defaults": {
    "unmatched": "local",
    "cooldown_seconds": 1,
    "browser_launch_timeout": 5
  }
}|} port

let test_rules =
  {|[
  {"pattern": "https?://www[.]example[.]com/.*", "target": "test-tenant", "enabled": true},
  {"pattern": "https?://disabled[.]example[.]com/.*", "target": "test-tenant", "enabled": false}
]|}

let find_free_port () =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt fd Unix.SO_REUSEADDR true;
  Unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  let port =
    match Unix.getsockname fd with
    | Unix.ADDR_INET (_, p) -> p
    | _ -> failwith "unexpected address"
  in
  Unix.close fd;
  port

let server_binary =
  match Stdlib.Sys.getenv_opt "ALLOYD_BIN" with
  | Some path -> path
  | None -> failwith "ALLOYD_BIN environment variable not set"

let start () =
  let port = find_free_port () in
  let config_dir = Stdlib.Filename.temp_dir "alloy-test-" "" in
  let config_path = config_dir ^ "/config.json" in
  let rules_path = config_dir ^ "/rules.json" in
  Out_channel.write_all config_path ~data:(test_config ~port);
  Out_channel.write_all rules_path ~data:test_rules;
  (* Start daemon, capture stdout to detect "listening" *)
  let (stdout_read, stdout_write) = Unix.pipe () in
  let dev_null_r = Unix.openfile "/dev/null" [Unix.O_RDONLY] 0 in
  let dev_null_w = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0 in
  let pid =
    Unix.create_process server_binary
      [| server_binary; "--config"; config_path |]
      dev_null_r stdout_write dev_null_w
  in
  Unix.close dev_null_r;
  Unix.close dev_null_w;
  Unix.close stdout_write;
  (* Wait for "listening" on stdout *)
  let ic = Unix.in_channel_of_descr stdout_read in
  let rec wait_for_ready () =
    let line = In_channel.input_line_exn ic in
    match String.is_substring line ~substring:"listening" with
    | true -> ()
    | false -> wait_for_ready ()
  in
  (try wait_for_ready ()
   with End_of_file -> failwith "daemon exited before becoming ready");
  (* Keep stderr_read open to avoid SIGPIPE killing the daemon *)
  { pid; port; config_dir }

let stop daemon =
  Unix.kill daemon.pid Stdlib.Sys.sigterm;
  let _status = Unix.waitpid [] daemon.pid in
  (* Clean up temp dir *)
  let entries = Stdlib.Sys.readdir daemon.config_dir in
  Array.iter entries ~f:(fun name ->
    Stdlib.Sys.remove (daemon.config_dir ^ "/" ^ name));
  Stdlib.Sys.rmdir daemon.config_dir

let connect daemon ~name () =
  let* transport = Tcp_transport.connect ~host:"127.0.0.1" ~port:daemon.port in
  let* (conn, push_stream) = Client.init
    ~recv_s:transport.recv_s
    ~send_f:transport.send_f
    ~name
    ()
  in
  Lwt.return (conn, push_stream, transport)
