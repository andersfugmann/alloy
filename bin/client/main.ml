open! Base
open! Stdio

(* -- CLI argument parsing *)

type cli_command =
  | Cli_cmd : {
      cmd : ('req, 'resp) Protocol.command;
      params : 'req;
      format : 'resp -> string;
    } -> cli_command

type cli_mode =
  | Cli_command of cli_command
  | Bridge
  | Register_stream

type cli_options = {
  mode : cli_mode;
  host : string;
  port : int;
  name : string option;
}

let parse_rules_file json_file =
  let content = In_channel.read_all json_file in
  match
    Result.bind (Protocol.parse_json_string content) ~f:Protocol.rules_of_yojson
  with
  | Ok rules -> rules
  | Error msg -> failwith (Printf.sprintf "invalid rules JSON: %s" msg)

let parse_config_file json_file =
  let content = In_channel.read_all json_file in
  match
    Result.bind (Protocol.parse_json_string content) ~f:Protocol.config_of_yojson
  with
  | Ok cfg -> cfg
  | Error msg -> failwith (Printf.sprintf "invalid config JSON: %s" msg)

(* Format response as human-readable CLI output *)

let format_route_result = function
  | Protocol.Local -> "Local"
  | Protocol.Remote tid -> Printf.sprintf "Remote: %s" tid

let format_test_result = function
  | Protocol.Match { tenant; rule_index } ->
    Printf.sprintf "Match: tenant=%s rule=%d" tenant rule_index
  | Protocol.No_match { default_tenant } ->
    Printf.sprintf "No match: default=%s" default_tenant

let format_status v =
  Printf.sprintf "Tenants: %s\nUptime: %ds"
    (String.concat ~sep:", " v.Protocol.registered_tenants)
    v.Protocol.uptime_seconds

let cli_term () =
  let open Cmdliner in
  let host_opt =
    let doc = "Daemon host address." in
    Arg.(value & opt string Constants.default_host
         & info [ "host"; "H" ] ~docv:"HOST" ~doc)
  in
  let port_opt =
    let doc = "Daemon port." in
    Arg.(value & opt int Constants.default_port
         & info [ "port"; "p" ] ~docv:"PORT" ~doc)
  in
  let name_opt =
    let doc = "Override tenant name." in
    Arg.(value & opt (some string) None
         & info [ "name"; "n" ] ~docv:"TENANT" ~doc)
  in
  let make_opts mode host port name = { mode; host; port; name } in
  let bridge_cmd =
    let doc = "Run as native messaging bridge for the browser extension." in
    Cmd.v (Cmd.info "bridge" ~doc)
      Term.(const (make_opts Bridge) $ host_opt $ port_opt $ name_opt)
  in
  let register_cmd =
    let doc = "Register as a tenant and stream push messages." in
    Cmd.v (Cmd.info "register" ~doc)
      Term.(const (make_opts Register_stream) $ host_opt $ port_opt $ name_opt)
  in
  let open_cmd =
    let doc = "Open a URL via the routing daemon." in
    let url =
      Arg.(required & pos 0 (some string) None
           & info [] ~docv:"URL" ~doc:"URL to open.")
    in
    Cmd.v (Cmd.info "open" ~doc)
      Term.(const (fun url -> make_opts (Cli_command (Cli_cmd { cmd = Open; params = { url }; format = format_route_result })))
            $ url $ host_opt $ port_opt $ name_opt)
  in
  let open_on_cmd =
    let doc = "Open a URL on a specific tenant." in
    let target =
      Arg.(required & pos 0 (some string) None
           & info [] ~docv:"TARGET" ~doc:"Target tenant.")
    in
    let url =
      Arg.(required & pos 1 (some string) None
           & info [] ~docv:"URL" ~doc:"URL to open.")
    in
    Cmd.v (Cmd.info "open-on" ~doc)
      Term.(const (fun target url ->
              make_opts (Cli_command (Cli_cmd { cmd = Open_on; params = { target; url }; format = format_route_result })))
            $ target $ url $ host_opt $ port_opt $ name_opt)
  in
  let test_cmd =
    let doc = "Test which tenant a URL would route to." in
    let url =
      Arg.(required & pos 0 (some string) None
           & info [] ~docv:"URL" ~doc:"URL to test.")
    in
    Cmd.v (Cmd.info "test" ~doc)
      Term.(const (fun url -> make_opts (Cli_command (Cli_cmd { cmd = Test; params = { url }; format = format_test_result })))
            $ url $ host_opt $ port_opt $ name_opt)
  in
  let get_config_cmd =
    let doc = "Get the current daemon configuration." in
    Cmd.v (Cmd.info "get-config" ~doc)
      Term.(const (make_opts (Cli_command (Cli_cmd { cmd = Get_config; params = (); format = fun c -> Yojson.Safe.pretty_to_string (Protocol.config_to_yojson c) })))
            $ host_opt $ port_opt $ name_opt)
  in
  let set_config_cmd =
    let doc = "Set the daemon configuration from a JSON file." in
    let json_file =
      Arg.(required & pos 0 (some string) None
           & info [] ~docv:"FILE" ~doc:"Path to JSON config file.")
    in
    Cmd.v (Cmd.info "set-config" ~doc)
      Term.(const (fun json_file ->
              make_opts (Cli_command (Cli_cmd { cmd = Set_config; params = parse_config_file json_file; format = fun () -> "OK" })))
            $ json_file $ host_opt $ port_opt $ name_opt)
  in
  let get_rules_cmd =
    let doc = "Get the current routing rules." in
    Cmd.v (Cmd.info "get-rules" ~doc)
      Term.(const (make_opts (Cli_command (Cli_cmd { cmd = Get_rules; params = (); format = fun r -> Yojson.Safe.pretty_to_string (Protocol.rules_to_yojson r) })))
            $ host_opt $ port_opt $ name_opt)
  in
  let set_rules_cmd =
    let doc = "Set routing rules from a JSON file." in
    let json_file =
      Arg.(required & pos 0 (some string) None
           & info [] ~docv:"FILE" ~doc:"Path to JSON rules file.")
    in
    Cmd.v (Cmd.info "set-rules" ~doc)
      Term.(const (fun json_file ->
              make_opts (Cli_command (Cli_cmd { cmd = Set_rules; params = parse_rules_file json_file; format = fun () -> "OK" })))
            $ json_file $ host_opt $ port_opt $ name_opt)
  in
  let status_cmd =
    let doc = "Show daemon status." in
    Cmd.v (Cmd.info "status" ~doc)
      Term.(const (make_opts (Cli_command (Cli_cmd { cmd = Status; params = (); format = format_status })))
            $ host_opt $ port_opt $ name_opt)
  in
  Cmd.group (Cmd.info "alloy" ~doc:"Alloy URL routing client")
    [ bridge_cmd; register_cmd; open_cmd; open_on_cmd; test_cmd;
      get_config_cmd; set_config_cmd; get_rules_cmd; set_rules_cmd;
      status_cmd ]

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

(* -- Send a command to the daemon and get a response (CLI) *)

let send_command_cli ~net ~tenant ~host ~port (Cli_cmd { cmd; params; format }) =
  Eio.Switch.run @@ fun sw ->
  let flow = connect_to_daemon ~sw net ~host ~port in
  let frame = Protocol.make_request_frame cmd params 1 (Some tenant) in
  Eio.Flow.copy_string (Protocol.serialize_frame frame ^ "\n") flow;
  let reader = Eio.Buf_read.of_flow ~max_size:Constants.max_read_buffer flow in
  let response_line = Eio.Buf_read.line reader in
  let ( let* ) r f = Result.bind r ~f in
  let result =
    let* frame = Protocol.deserialize_frame response_line in
    let* rp = Protocol.parse_response_payload frame in
    match rp with
    | Protocol.Success json -> Protocol.response_deserializer cmd json
    | Protocol.Failure msg -> Error msg
  in
  match result with
  | Ok value -> format value
  | Error msg -> Printf.sprintf "Error: %s" msg

(* -- CLI register: stay connected, print pushes *)

let parse_push line =
  let ( let* ) r f = Result.bind r ~f in
  let* frame = Protocol.deserialize_frame line in
  match frame.id with
  | 0 -> Protocol.parse_push_payload frame
  | _ -> Error "unexpected non-push message"

let run_register ~net ~host ~port ~tenant =
  Eio.Switch.run @@ fun sw ->
  let flow =
    connect_to_daemon ~sw net ~host ~port
  in
  let frame = Protocol.make_request_frame Register { brand = None; address = None; name = None } 0 (Some tenant) in
  Eio.Flow.copy_string (Protocol.serialize_frame frame ^ "\n") flow;
  let reader = Eio.Buf_read.of_flow ~max_size:Constants.max_read_buffer flow in
  let first_line = Eio.Buf_read.line reader in
  (match parse_push first_line with
   | Ok (Registered { tenant_id }) ->
     printf "Registered as %s\n%!" tenant_id
   | Ok _ ->
     eprintf "Unexpected push during registration\n%!";
     Stdlib.exit 1
   | Error msg ->
     eprintf "Registration parse error: %s\n%!" msg;
     Stdlib.exit 1);
  let rec read_loop () =
    match Eio.Buf_read.line reader with
    | line ->
      (match parse_push line with
       | Ok (Navigate { url }) ->
         printf "NAVIGATE %s\n%!" url
       | Ok (Config_updated { config = cfg; registered_tenants }) ->
         printf "CONFIG_UPDATED tenants=%d registered=%d\n%!"
           (List.length cfg.tenants) (List.length registered_tenants)
       | Ok (Registered { tenant_id }) ->
         printf "RE-REGISTERED %s\n%!" tenant_id
       | Error msg ->
         eprintf "Parse error: %s\n%!" msg);
      read_loop ()
    | exception End_of_file ->
      eprintf "Server disconnected\n%!"
    | exception Eio.Io _ ->
      eprintf "Server disconnected\n%!"
  in
  read_loop ()

(* -- Native messaging framing *)

let read_native_message_raw source : string option =
  let len_buf = Cstruct.create 4 in
  match Eio.Flow.read_exact source len_buf with
  | exception End_of_file -> None
  | exception Eio.Io _ -> None
  | () ->
    let len = Cstruct.LE.get_uint32 len_buf 0 |> Int32.to_int_exn in
    let data_buf = Cstruct.create len in
    begin match Eio.Flow.read_exact source data_buf with
    | exception End_of_file -> None
    | exception Eio.Io _ -> None
    | () -> Some (Cstruct.to_string data_buf)
    end

let read_native_message source : Yojson.Safe.t option =
  read_native_message_raw source
  |> Option.bind ~f:(fun s ->
    match Yojson.Safe.from_string s with
    | json -> Some json
    | exception Yojson.Json_error _ -> None)

let write_native_message_raw sink (data : string) : unit =
  let len = String.length data in
  let len_buf = Cstruct.create 4 in
  Cstruct.LE.set_uint32 len_buf 0 (Int32.of_int_exn len);
  Eio.Flow.copy_string (Cstruct.to_string len_buf ^ data) sink

(* -- Bridge mode: transparent relay *)

let run_bridge env =
  let net = Eio.Stdenv.net env in
  let default_tenant = Unix.gethostname () in
  let stdout_flow = Eio.Stdenv.stdout env in
  let stdin_flow = Eio.Stdenv.stdin env in
  let stdout_stream = Eio.Stream.create Constants.bridge_stream_capacity in
  (* Wait for a valid Register request; reject anything else *)
  let err_not_registered id =
    Protocol.make_response_frame id (Error "Not connected. Send Register first")
    |> Protocol.serialize_frame
  in
  let rec await_register () =
    match read_native_message stdin_flow with
    | None -> None
    | Some json ->
      match Protocol.frame_of_yojson json with
      | Error _ ->
        Eio.Stream.add stdout_stream (err_not_registered 0);
        await_register ()
      | Ok frame ->
        match Protocol.parse_request_payload frame with
        | Error _ ->
          Eio.Stream.add stdout_stream (err_not_registered frame.id);
          await_register ()
        | Ok rp ->
          match String.equal rp.command "register" with
          | false ->
            Eio.Stream.add stdout_stream (err_not_registered frame.id);
            await_register ()
          | true ->
            let (name, address, brand) =
              match Protocol.register_request_of_yojson rp.params with
              | Ok p -> (p.name, p.address, p.brand)
              | Error _ -> (None, None, None)
            in
            let tenant = Option.value name ~default:default_tenant in
            let patched_request : Protocol.register_request = {
              brand;
              address = None;
              name = Some tenant;
            } in
            let patched_frame = Protocol.make_request_frame Register patched_request 0 (Some tenant) in
            Some (tenant, address, Protocol.serialize_frame patched_frame)
  in
  match await_register () with
  | None -> ()
  | Some (_tenant, addr_override, register_line) ->
  let host, port =
    match addr_override with
    | Some s ->
      (match String.rsplit2 s ~on:':' with
       | Some (h, p) -> (h, Option.value (Int.of_string_opt p) ~default:Constants.default_port)
       | None -> (s, Constants.default_port))
    | None -> (Constants.default_host, Constants.default_port)
  in
  (* stdout writer: single writer ensures no interleaving *)
  let write_stdout () =
    let rec loop () =
      let s = Eio.Stream.take stdout_stream in
      write_native_message_raw stdout_flow s;
      loop ()
    in
    loop ()
  in
  (* TCP relay — exit on disconnect *)
  let relay () =
    match
      Eio.Switch.run @@ fun sw ->
      let flow = connect_to_daemon ~sw net ~host ~port in
      Eio.Flow.copy_string (register_line ^ "\n") flow;
      let reader = Eio.Buf_read.of_flow ~max_size:Constants.max_read_buffer flow in
      (* Forward first message (Registered push) to extension *)
      let first_line = Eio.Buf_read.line reader in
      Eio.Stream.add stdout_stream first_line;
      (* Transparent relay: no parsing, no ID management *)
      Eio.Fiber.both
        (fun () ->
          let rec read_tcp () =
            let line = Eio.Buf_read.line reader in
            Eio.Stream.add stdout_stream line;
            read_tcp ()
          in
          read_tcp ())
        (fun () ->
          let rec read_stdin () =
            match read_native_message_raw stdin_flow with
            | None -> ()
            | Some data ->
              Eio.Flow.copy_string (data ^ "\n") flow;
              read_stdin ()
          in
          read_stdin ())
    with
    | () -> ()
    | exception exn ->
      eprintf "Bridge: %s\n%!" (Exn.to_string exn)
  in
  Eio.Fiber.both write_stdout relay

(* -- Main *)

let run_cli { mode; host; port; name } =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let resolve_tenant default = Option.value name ~default in
  match mode with
  | Bridge -> run_bridge env
  | Register_stream ->
    run_register ~net ~host ~port ~tenant:(resolve_tenant (Unix.gethostname ()))
  | Cli_command cli_cmd ->
    let tenant = resolve_tenant "default" in
    let output = send_command_cli ~net ~tenant ~host ~port cli_cmd in
    print_endline output

let () =
  let argv = Sys.get_argv () in
  (* Chromium launches native messaging hosts with a chrome-extension:// origin arg *)
  match Array.length argv with
  | 2 when String.is_prefix (Array.get argv 1) ~prefix:"chrome-extension://" ->
    run_cli { mode = Bridge; host = Constants.default_host; port = Constants.default_port; name = None }
  | _ ->
    (match Cmdliner.Cmd.eval_value (cli_term ()) with
     | Ok (`Ok opts) -> run_cli opts
     | Ok `Help | Ok `Version -> ()
     | Error _ -> Stdlib.exit 1)
