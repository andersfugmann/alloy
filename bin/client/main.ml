open! Base
open! Stdio

(* -- Config file resolution *)

let config_path () =
  let home = Sys.getenv "HOME" |> Option.value ~default:"" in
  home ^ "/.config/alloy/config.json"

type resolved_config = {
  host : string;
  port : int;
}

let read_config_file () : resolved_config option =
  let path = config_path () in
  match In_channel.read_all path with
  | content ->
    begin match Protocol.parse_json_string content with
    | Error _ -> None
    | Ok json ->
      match Protocol.config_of_yojson json with
      | Error _ -> None
      | Ok config ->
        match config.listen with
        | addr :: _ -> Some { host = addr.host; port = addr.port }
        | [] -> None
    end
  | exception Sys_error _ -> None

(* -- Connect to daemon helper *)

let resolve_host host =
  match Unix.inet_addr_of_string host with
  | addr -> addr
  | exception Failure _ ->
    let entry = Unix.gethostbyname host in
    entry.Unix.h_addr_list.(0)

let connect_to_daemon ~sw net ~host ~port =
  let ip = Eio_unix.Net.Ipaddr.of_unix (resolve_host host) in
  Eio.Net.connect ~sw net (`Tcp (ip, port))

(* -- Main logic *)

let run ~url ~host ~port =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let flow = connect_to_daemon ~sw net ~host ~port in
  let frame = Protocol.make_request_frame Open { url } 1 in
  Eio.Flow.copy_string (Protocol.serialize_frame frame ^ "\n") flow;
  let reader = Eio.Buf_read.of_flow ~max_size:Constants.max_read_buffer flow in
  let response_line = Eio.Buf_read.line reader in
  let ( let* ) r f = Result.bind r ~f in
  let result =
    let* frame = Protocol.deserialize_frame response_line in
    let* rp = Protocol.parse_response_payload frame in
    match rp with
    | Protocol.Success json -> Protocol.response_deserializer Open json
    | Protocol.Failure msg -> Error msg
  in
  match result with
  | Ok Protocol.Local -> print_endline "Local"
  | Ok (Protocol.Remote tid) -> printf "Remote: %s\n" tid
  | Error msg ->
    eprintf "Error: %s\n" msg;
    Stdlib.exit 1

(* -- CLI *)

let () =
  let open Cmdliner in
  let url_arg =
    let doc = "URL to open via the routing daemon." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"URL" ~doc)
  in
  let host_opt =
    let doc = "Daemon host address. Overrides config file." in
    Arg.(value & opt (some string) None & info [ "host"; "H" ] ~docv:"HOST" ~doc)
  in
  let port_opt =
    let doc = "Daemon port. Overrides config file." in
    Arg.(value & opt (some int) None & info [ "port"; "p" ] ~docv:"PORT" ~doc)
  in
  let run_cmd url host_override port_override =
    let file_config = read_config_file () in
    let host =
      match host_override with
      | Some h -> h
      | None ->
        match file_config with
        | Some c -> c.host
        | None -> Constants.default_host
    in
    let port =
      match port_override with
      | Some p -> p
      | None ->
        match file_config with
        | Some c -> c.port
        | None -> Constants.default_port
    in
    run ~url ~host ~port
  in
  let cmd =
    let doc = "Open a URL via the Alloy routing daemon." in
    Cmd.v (Cmd.info "alloy_open" ~doc)
      Term.(const run_cmd $ url_arg $ host_opt $ port_opt)
  in
  match Cmd.eval_value cmd with
  | Ok (`Ok ()) -> ()
  | Ok `Help | Ok `Version -> ()
  | Error _ -> Stdlib.exit 1
