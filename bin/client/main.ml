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

let resolve_connection host_override port_override =
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
  (host, port)

let send_request : type req resp. host:string -> port:int -> (req, resp) Protocol.command -> req -> (resp, string) Result.t =
  fun ~host ~port cmd request ->
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let flow = connect_to_daemon ~sw net ~host ~port in
  let frame = Protocol.make_request_frame cmd request 1 in
  Eio.Flow.copy_string (Protocol.serialize_frame frame ^ "\n") flow;
  let reader = Eio.Buf_read.of_flow ~max_size:Constants.max_read_buffer flow in
  let response_line = Eio.Buf_read.line reader in
  let ( let* ) r f = Result.bind r ~f in
  let* frame = Protocol.deserialize_frame response_line in
  let* rp = Protocol.response_payload_of_yojson frame.Protocol.payload in
  match rp with
  | Ok json -> Protocol.response_deserializer cmd json
  | Error msg -> Error msg

(* -- Open command *)

let run_open ~url ~host ~port =
  match send_request ~host ~port Open { url; title = None } with
  | Ok Protocol.Local -> print_endline "Local"
  | Ok (Protocol.Remote tid) -> printf "Remote: %s\n" tid
  | Error msg ->
    eprintf "Error: %s\n" msg;
    Stdlib.exit 1

(* -- Firefox import *)

let firefox_dir () =
  let home = Sys.getenv "HOME" |> Option.value ~default:"" in
  home ^ "/.mozilla/firefox"

let find_firefox_profile ?profile () =
  let base = firefox_dir () in
  match profile with
  | Some name -> base ^ "/" ^ name
  | None ->
    let entries =
      match Stdlib.Sys.readdir base with
      | arr -> Array.to_list arr
      | exception Sys_error _ -> []
    in
    let candidates =
      List.filter entries ~f:(fun e ->
        String.is_suffix e ~suffix:".default-release"
        || String.is_suffix e ~suffix:".default")
      |> List.sort ~compare:(fun a b ->
        match String.is_suffix a ~suffix:".default-release",
              String.is_suffix b ~suffix:".default-release" with
        | true, false -> -1
        | false, true -> 1
        | _ -> String.compare a b)
    in
    match candidates with
    | name :: _ -> base ^ "/" ^ name
    | [] -> failwith "no Firefox profile found; use --profile or --db"

let copy_file src dst =
  let ic = In_channel.create ~binary:true src in
  let data = In_channel.input_all ic in
  In_channel.close ic;
  Out_channel.write_all dst ~data

let read_firefox_history db_path =
  let tmp = Stdlib.Filename.temp_file "alloy-firefox-" ".sqlite" in
  let tmp_wal = tmp ^ "-wal" in
  let tmp_shm = tmp ^ "-shm" in
  let finally () =
    List.iter [tmp; tmp_wal; tmp_shm] ~f:(fun f ->
      (try Stdlib.Sys.remove f with _ -> ()))
  in
  match
    copy_file db_path tmp;
    let wal_path = db_path ^ "-wal" in
    let shm_path = db_path ^ "-shm" in
    (match Stdlib.Sys.file_exists wal_path with
     | true -> copy_file wal_path tmp_wal
     | false -> ());
    (match Stdlib.Sys.file_exists shm_path with
     | true -> copy_file shm_path tmp_shm
     | false -> ());
    let db = Sqlite3.db_open ~mode:`READONLY tmp in
    let entries = ref [] in
    let sql =
      "SELECT p.url, p.title, v.visit_date \
       FROM moz_places p \
       JOIN moz_historyvisits v ON p.id = v.place_id \
       WHERE p.url LIKE 'http%'"
    in
    let _rc = Sqlite3.exec db sql ~cb:(fun row _headers ->
      let url = Option.value row.(0) ~default:"" in
      let title = Option.value row.(1) ~default:"" in
      let visit_date_str = Option.value row.(2) ~default:"0" in
      match String.is_empty url with
      | true -> ()
      | false ->
        let visit_day =
          Int64.of_string_opt visit_date_str
          |> Option.value ~default:0L
          |> fun us -> Int64.to_int_trunc (Int64.( / ) us 86_400_000_000L)
        in
        entries := (url, title, visit_day) :: !entries)
    in
    (match Sqlite3.db_close db with true -> () | false -> ());
    let by_url = Hashtbl.create (module String) in
    List.iter !entries ~f:(fun (url, title, day) ->
      let (prev_title, days) =
        Hashtbl.find by_url url
        |> Option.value ~default:("", Set.empty (module Int))
      in
      let best_title =
        match String.is_empty prev_title with
        | true -> title
        | false -> prev_title
      in
      Hashtbl.set by_url ~key:url ~data:(best_title, Set.add days day));
    Hashtbl.fold by_url ~init:[] ~f:(fun ~key:url ~data:(title, days) acc ->
      let visits = Set.to_list days |> List.rev in
      Protocol.{ url; title; visits } :: acc)
  with
  | result -> finally (); result
  | exception exn -> finally (); raise exn

let run_import_firefox ~profile ~db_path ~host ~port =
  let places_path =
    match db_path with
    | Some p -> p
    | None ->
      let profile_dir = find_firefox_profile ?profile () in
      profile_dir ^ "/places.sqlite"
  in
  (match Stdlib.Sys.file_exists places_path with
   | false -> failwith (Printf.sprintf "database not found: %s" places_path)
   | true -> ());
  printf "Reading Firefox history from %s\n" places_path;
  let entries = read_firefox_history places_path in
  printf "Found %d unique URLs\n" (List.length entries);
  match List.is_empty entries with
  | true -> print_endline "Nothing to import."
  | false ->
    match send_request ~host ~port Import_history entries with
    | Ok count -> printf "Import complete. History now contains %d entries.\n" count
    | Error msg ->
      eprintf "Error: %s\n" msg;
      Stdlib.exit 1

(* -- CLI *)

let () =
  let open Cmdliner in
  let host_opt =
    let doc = "Daemon host address. Overrides config file." in
    Arg.(value & opt (some string) None & info [ "host"; "H" ] ~docv:"HOST" ~doc)
  in
  let port_opt =
    let doc = "Daemon port. Overrides config file." in
    Arg.(value & opt (some int) None & info [ "port"; "p" ] ~docv:"PORT" ~doc)
  in
  let open_cmd =
    let url_arg =
      let doc = "URL to open via the routing daemon." in
      Arg.(required & pos 0 (some string) None & info [] ~docv:"URL" ~doc)
    in
    let run_cmd url host_override port_override =
      let (host, port) = resolve_connection host_override port_override in
      run_open ~url ~host ~port
    in
    let doc = "Open a URL via the Alloy routing daemon." in
    Cmd.v (Cmd.info "open" ~doc)
      Term.(const run_cmd $ url_arg $ host_opt $ port_opt)
  in
  let import_firefox_cmd =
    let profile_opt =
      let doc = "Firefox profile name (e.g., default-release). Auto-detected if omitted." in
      Arg.(value & opt (some string) None & info [ "profile" ] ~docv:"NAME" ~doc)
    in
    let db_opt =
      let doc = "Direct path to places.sqlite. Overrides profile detection." in
      Arg.(value & opt (some string) None & info [ "db" ] ~docv:"PATH" ~doc)
    in
    let run_cmd profile db_path host_override port_override =
      let (host, port) = resolve_connection host_override port_override in
      run_import_firefox ~profile ~db_path ~host ~port
    in
    let doc = "Import Firefox browsing history into Alloy." in
    Cmd.v (Cmd.info "import-firefox" ~doc)
      Term.(const run_cmd $ profile_opt $ db_opt $ host_opt $ port_opt)
  in
  let main_cmd =
    let doc = "Alloy URL routing client." in
    Cmd.group (Cmd.info "alloy" ~doc)
      [ open_cmd; import_firefox_cmd ]
  in
  match Cmd.eval_value main_cmd with
  | Ok (`Ok ()) -> ()
  | Ok `Help | Ok `Version -> ()
  | Error _ -> Stdlib.exit 1
