open! Base
open! Stdio

let log msg = Stdlib.Printf.eprintf "[alloy_bridge] %s\n%!" msg

(* -- Native messaging I/O (Chrome length-prefixed format) *)

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

(* -- TCP connection *)

let connect_to_daemon ~sw net ~host ~port =
  let addr = `Tcp (Eio.Net.Ipaddr.of_raw host, port) in
  Eio.Net.connect ~sw net addr

(* -- Main bridge logic *)

let run env =
  let net = Eio.Stdenv.net env in
  let hostname = Unix.gethostname () in
  log (Stdlib.Printf.sprintf "starting (hostname=%s, pid=%d)" hostname (Unix.getpid ()));
  let stdin_flow = Eio.Stdenv.stdin env in
  let stdout_flow = Eio.Stdenv.stdout env in
  let stdout_stream = Eio.Stream.create Constants.bridge_stream_capacity in
  (* Phase 1: Wait for connect handshake from extension *)
  let handshake () : Protocol.listen_address option =
    let send_error msg =
      log (Stdlib.Printf.sprintf "handshake error: %s" msg);
      let resp = Protocol.make_bridge_error msg in
      write_native_message_raw stdout_flow
        (Yojson.Safe.to_string (Protocol.bridge_response_to_yojson resp))
    in
    log "waiting for handshake";
    let rec await () =
      match read_native_message stdin_flow with
      | None ->
        log "stdin closed before handshake";
        None
      | Some json ->
        log (Stdlib.Printf.sprintf "received: %s" (Yojson.Safe.to_string json));
        match Protocol.parse_bridge_request json with
        | Ok addr -> Some addr
        | Error msg ->
          send_error msg;
          await ()
    in
    await ()
  in
  match handshake () with
  | None -> log "exiting (no handshake)"
  | Some addr ->
  let host = addr.host in
  let port = addr.port in
  log (Stdlib.Printf.sprintf "connecting to daemon at %s:%d" host port);
  (* Phase 2: Connect to daemon *)
  let write_stdout () =
    let rec loop () =
      let s = Eio.Stream.take stdout_stream in
      write_native_message_raw stdout_flow s;
      loop ()
    in
    loop ()
  in
  let relay () =
    match
      Eio.Switch.run @@ fun sw ->
      let flow = connect_to_daemon ~sw net ~host ~port in
      log "connected to daemon";
      (* Send connected response to extension *)
      let resp = Protocol.make_bridge_connected hostname in
      Eio.Stream.add stdout_stream
        (Yojson.Safe.to_string (Protocol.bridge_response_to_yojson resp));
      let reader = Eio.Buf_read.of_flow ~max_size:Constants.max_read_buffer flow in
      (* Transparent relay *)
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
            | None ->
              log "stdin closed, shutting down";
              ()
            | Some data ->
              Eio.Flow.copy_string (data ^ "\n") flow;
              read_stdin ()
          in
          read_stdin ())
    with
    | () -> log "relay finished"
    | exception exn ->
      log (Stdlib.Printf.sprintf "relay error: %s" (Exn.to_string exn));
      let resp = Protocol.make_bridge_error (Exn.to_string exn) in
      Eio.Stream.add stdout_stream
        (Yojson.Safe.to_string (Protocol.bridge_response_to_yojson resp))
  in
  Eio.Fiber.both write_stdout relay

let () = Eio_main.run run
