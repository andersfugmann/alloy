open! Base
open! Stdio
open! Log

(* -- State *)

type cooldown_entry = { key : string; expires : float }

type pending_delivery = {
  url : string;
  reply : (Protocol.route_result, string) Result.t Eio.Promise.u;
  promise : (Protocol.route_result, string) Result.t Eio.Promise.t;
}

type starting_tenant = {
  pending : pending_delivery list;
}

type compiled_rule = {
  rule : Protocol.rule;
  regex : Re.re;
}

type state = {
  config : Protocol.config;
  config_path : string;
  compiled_rules : compiled_rule list;
  registry : string Eio.Stream.t Map.M(String).t;
  starting : (string * starting_tenant) list;
  cooldowns : cooldown_entry list;
  start_time : float;
}

(* -- Coordinator messages *)

type coordinator_msg =
  | Dispatch of {
      id : int;
      command : Protocol.packed_command;
      tenant : string;
      reply : string Eio.Promise.u;
    }
  | Register_tenant of {
      tenant : string;
      brand : string option;
      push_stream : string Eio.Stream.t;
      reply : (string, string) Result.t Eio.Promise.u;
    }
  | Unregister_tenant of { tenant : string }
  | Launch_timeout of { tenant : string }

let default_config () : Protocol.config =
  {
    listen = Constants.default_listen;

    allowed_networks = Constants.default_allowed_networks;
    tenants = [];
    rules = [];
    defaults =
      { unmatched = "local"; cooldown_seconds = 5; browser_launch_timeout = 10 };
  }

(* -- Config loading / saving *)

let browser_cmd_of_brand brand =
  Option.bind brand ~f:(fun raw ->
      match String.lowercase raw with
      | b when String.is_substring b ~substring:"edge" -> Some "microsoft-edge"
      | b when String.is_substring b ~substring:"chromium" -> Some "chromium"
      | b when String.is_substring b ~substring:"chrome" -> Some "chrome"
      | _ -> None)

let rec mkdir_p path =
  match Stdlib.Sys.file_exists path with
  | true -> ()
  | false ->
    mkdir_p (Stdlib.Filename.dirname path);
    (try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let save_config_to_path config_path config =
  mkdir_p (Stdlib.Filename.dirname config_path);
  let json = Protocol.config_to_yojson config in
  let content = Yojson.Safe.pretty_to_string json in
  Out_channel.write_all config_path ~data:(content ^ "\n")

let load_config path =
  match Stdlib.Sys.file_exists path with
  | true ->
    let content = In_channel.read_all path in
    Result.bind (Protocol.parse_json_string content) ~f:(fun json ->
        Protocol.config_of_yojson json)
  | false ->
    let config = default_config () in
    log "no config found, creating default at %s" path;
    (try
       save_config_to_path path config
     with exn ->
       log "warning: could not write default config: %s"
         (Exn.to_string exn));
    Ok config

(* -- Rule evaluation *)

let compile_regex pattern =
  match Re.compile (Re.Pcre.re pattern) with
  | regex -> Ok regex
  | exception exn ->
    Error (Printf.sprintf "invalid regex '%s': %s" pattern (Exn.to_string exn))

let compile_rule (rule : Protocol.rule) =
  compile_regex rule.pattern
  |> Result.map ~f:(fun regex -> { rule; regex })

let compile_rules rules =
  List.map rules ~f:compile_rule
  |> Result.all

let find_matching_rule compiled_rules url =
  List.find_mapi ~f:(fun i cr ->
      match cr.rule.enabled && Re.execp cr.regex url with
      | true -> Some (cr.rule.target, i)
      | false -> None
    ) compiled_rules

(* -- Cooldown *)

let check_and_prune_cooldowns cooldowns ~now ~key =
  let rec loop acc = function
    | [] -> (false, List.rev acc)
    | entry :: _ when Float.(entry.expires < now) ->
      (false, List.rev acc)
    | entry :: rest when String.equal entry.key key ->
      (true, List.rev_append (entry :: acc) rest)
    | entry :: rest ->
      loop (entry :: acc) rest
  in
  loop [] cooldowns

(* -- Launch browser process (fire-and-forget) *)

let launch_browser cmd =
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0o000 in
  let pid = Unix.create_process "/bin/sh" [| "/bin/sh"; "-c"; cmd |] dev_null dev_null dev_null in
  Unix.close dev_null;
  log "launched browser (pid %d): %s" pid cmd

(* -- Deliver URL to tenant (idempotent, may defer) *)

let deliver_url state target url ~sw ~clock ~inbox =
  match Map.find state.registry target with
  | Some stream ->
    let push_msg = Protocol.Wire.Push { id = 0; push = Navigate { url } } in
    Eio.Stream.add stream (Protocol.serialize_server_message push_msg);
    (state, Eio.Promise.create_resolved (Ok (Protocol.Remote target)))
  | None ->
    let existing_sentinel = List.Assoc.find state.starting ~equal:String.equal target in
    let sentinel =
      match existing_sentinel with
      | Some _ -> existing_sentinel
      | None ->
        List.Assoc.find state.config.tenants ~equal:String.equal target
        |> Option.bind ~f:(fun (tc : Protocol.tenant_config) -> tc.browser_cmd)
        |> Option.map ~f:(fun cmd ->
             let timeout = Float.of_int state.config.defaults.browser_launch_timeout in
             log "starting tenant %s (timeout %.0fs): %s" target timeout cmd;
             Eio.Fiber.fork ~sw (fun () ->
                 launch_browser cmd;
                 Eio.Time.sleep clock timeout;
                 Eio.Stream.add inbox (Launch_timeout { tenant = target }));
             { pending = [] })
    in
    match sentinel with
    | None ->
      let msg = Printf.sprintf "Unknown tenant %s or no browser command given" target in
      (state, Eio.Promise.create_resolved (Error msg))
    | Some { pending } ->
      let pending, promise =
        match List.find pending ~f:(fun pd -> String.equal pd.url url) with
        | Some { promise; _ } ->
          log "URL already queued for starting tenant %s: %s" target url;
          (pending, promise)
        | None ->
          let (promise, resolver) = Eio.Promise.create () in
          ({ url; reply = resolver; promise } :: pending, promise)
      in
      let starting = List.Assoc.add state.starting ~equal:String.equal target { pending } in
      ({ state with starting }, promise)

(* -- Command handlers *)

let handle_open state tenant url ~sw ~clock ~inbox =
  let target, _ =
    Option.value ~default:(state.config.defaults.unmatched, 0) (find_matching_rule state.compiled_rules url)
  in
  let now = Unix.gettimeofday () in
  let (in_cooldown, pruned) = check_and_prune_cooldowns state.cooldowns ~now ~key:url in
  let state = { state with cooldowns = pruned } in
  let target =
    match in_cooldown || String.equal target tenant with
    | true -> "local"
    | false -> target
  in
  let cooldowns =
    match String.equal target "local" with
    | true -> state.cooldowns
    | false ->
      let cooldown = Float.of_int state.config.defaults.cooldown_seconds in
      { key = url; expires = now +. cooldown } :: state.cooldowns
  in
  let state = { state with cooldowns } in
  match String.equal target "local" with
  | true -> (state, Eio.Promise.create_resolved (Ok Protocol.Local))
  | false -> deliver_url state target url ~sw ~clock ~inbox

let handle_open_on state target url ~sw ~clock ~inbox =
  match String.equal target "local" with
  | true -> (state, Eio.Promise.create_resolved (Ok Protocol.Local))
  | false -> deliver_url state target url ~sw ~clock ~inbox

let handle_test state url =
  let result =
    match find_matching_rule state.compiled_rules url with
    | Some (target, idx) -> Protocol.Match { tenant = target; rule_index = idx }
    | None -> Protocol.No_match { default_tenant = state.config.defaults.unmatched }
  in
  (state, Ok result)

let handle_status state =
  let tenants = Map.keys state.registry in
  let uptime =
    Unix.gettimeofday () -. state.start_time |> Float.to_int
  in
  (state, Ok { Protocol.registered_tenants = tenants; uptime_seconds = uptime })

let try_save_config state =
  try
    save_config_to_path state.config_path state.config;
    Ok ()
  with exn ->
    Error (Printf.sprintf "failed to save config: %s" (Exn.to_string exn))

let handle_set_config state (cfg : Protocol.config) =
  match compile_rules cfg.rules with
  | Error msg -> (state, Error (Printf.sprintf "invalid rules: %s" msg))
  | Ok compiled_rules ->
    let state = { state with config = cfg; compiled_rules } in
    (state, try_save_config state)

let handle_add_rule state (rule : Protocol.rule) =
  match compile_rule rule with
  | Error msg -> (state, Error msg)
  | Ok compiled ->
    let config = { state.config with rules = state.config.rules @ [ rule ] } in
    let compiled_rules = state.compiled_rules @ [ compiled ] in
    let state = { state with config; compiled_rules } in
    (state, try_save_config state)

let handle_update_rule state idx (rule : Protocol.rule) =
  match compile_rule rule with
  | Error msg -> (state, Error msg)
  | Ok compiled ->
    let config = state.config in
    let len = List.length config.rules in
    (match idx >= 0 && idx < len with
     | false ->
       (state, Error (Printf.sprintf "rule index %d out of range (0..%d)" idx (len - 1)))
     | true ->
       let new_rules =
         List.mapi config.rules ~f:(fun i r ->
             match Int.equal i idx with true -> rule | false -> r)
       in
       let compiled_rules =
         List.mapi state.compiled_rules ~f:(fun i cr ->
             match Int.equal i idx with true -> compiled | false -> cr)
       in
       let config = { config with rules = new_rules } in
       let state = { state with config; compiled_rules } in
       (state, try_save_config state))

let handle_delete_rule state idx =
  let config = state.config in
  let len = List.length config.rules in
  match idx >= 0 && idx < len with
  | false ->
    (state, Error (Printf.sprintf "rule index %d out of range (0..%d)" idx (len - 1)))
  | true ->
    let new_rules =
      List.filteri config.rules ~f:(fun i _ -> not (Int.equal i idx))
    in
    let compiled_rules =
      List.filteri state.compiled_rules ~f:(fun i _ -> not (Int.equal i idx))
    in
    let config = { config with rules = new_rules } in
    let state = { state with config; compiled_rules } in
    (state, try_save_config state)

(* -- Command dispatch *)

let resolve_reply reply tenant id response_json =
  let msg = Protocol.Wire.Response { id; response = response_json } in
  let s = Protocol.serialize_server_message msg in
  log "res[%s]: %s" tenant s;
  Eio.Promise.resolve reply s

let broadcast_config (state : state) : unit =
  let registered = Map.keys state.registry in
  let push_wire = Protocol.Wire.Config_updated { config = state.config; registered_tenants = registered } in
  let msg = Protocol.Wire.Push { id = 0; push = push_wire } in
  let s = Protocol.serialize_server_message msg in
  Map.iter state.registry ~f:(fun stream ->
    Eio.Stream.add stream s)

let dispatch_command :
    type a. state -> Protocol.tenant_id -> int -> a Protocol.command ->
    reply:string Eio.Promise.u ->
    sw:Eio.Switch.t -> clock:_ Eio.Time.clock ->
    inbox:coordinator_msg Eio.Stream.t -> state =
 fun state tenant id cmd ~reply ~sw ~clock ~inbox ->
  let resolve resp = resolve_reply reply tenant id (Protocol.response_to_wire cmd resp) in
  match cmd with
  | Protocol.Register _ ->
    resolve (Error "unexpected Register in command mode");
    state
  | Protocol.Open url ->
    let (state, promise) = handle_open state tenant url ~sw ~clock ~inbox in
    Eio.Fiber.fork ~sw (fun () ->
      let result = Eio.Promise.await promise in
      resolve result);
    state
  | Protocol.Open_on (target, url) ->
    let (state, promise) = handle_open_on state target url ~sw ~clock ~inbox in
    Eio.Fiber.fork ~sw (fun () ->
      let result = Eio.Promise.await promise in
      resolve result);
    state
  | Protocol.Test url ->
    let (state, resp) = handle_test state url in
    resolve resp;
    state
  | Protocol.Get_config ->
    resolve (Ok state.config);
    state
  | Protocol.Set_config cfg ->
    let (state, resp) = handle_set_config state cfg in
    resolve resp;
    Result.iter resp ~f:(fun () -> broadcast_config state);
    state
  | Protocol.Add_rule rule ->
    let (state, resp) = handle_add_rule state rule in
    resolve resp;
    Result.iter resp ~f:(fun () -> broadcast_config state);
    state
  | Protocol.Update_rule (idx, rule) ->
    let (state, resp) = handle_update_rule state idx rule in
    resolve resp;
    Result.iter resp ~f:(fun () -> broadcast_config state);
    state
  | Protocol.Delete_rule idx ->
    let (state, resp) = handle_delete_rule state idx in
    resolve resp;
    Result.iter resp ~f:(fun () -> broadcast_config state);
    state
  | Protocol.Status ->
    let (state, resp) = handle_status state in
    resolve resp;
    state

(* -- Coordinator loop *)

let flush_pending_deliveries state tenant push_stream =
  match List.Assoc.find state.starting ~equal:String.equal tenant with
  | None -> state
  | Some sentinel ->
    List.iter sentinel.pending ~f:(fun pd ->
      let push_msg = Protocol.Wire.Push { id = 0; push = Navigate { url = pd.url } } in
      Eio.Stream.add push_stream (Protocol.serialize_server_message push_msg);
      Eio.Promise.resolve pd.reply (Ok (Protocol.Remote tenant));
      log "delivered pending URL to %s: %s" tenant pd.url);
    let starting = List.Assoc.remove state.starting ~equal:String.equal tenant in
    { state with starting }

let update_tenant_config state tenant brand =
  let suggested_cmd = browser_cmd_of_brand brand in
  let tenants =
    match List.Assoc.find state.config.tenants ~equal:String.equal tenant with
    | Some existing ->
      let browser_cmd =
        match existing.browser_cmd with
        | Some _ -> existing.browser_cmd
        | None -> suggested_cmd
      in
      let updated = { existing with brand; browser_cmd } in
      List.Assoc.add state.config.tenants ~equal:String.equal tenant updated
    | None ->
      let new_tenant : Protocol.tenant_config =
        { browser_cmd = suggested_cmd; label = tenant; color = "#808080"; brand }
      in
      log "auto-added tenant %s to config" tenant;
      state.config.tenants @ [ (tenant, new_tenant) ]
  in
  let config = { state.config with tenants } in
  let state = { state with config } in
  let _ = try_save_config state in
  state

let rec coordinator_loop state inbox ~sw ~clock =
  let state = match Eio.Stream.take inbox with
    | Dispatch { id; command = Protocol.Command cmd; tenant; reply } ->
      dispatch_command state tenant id cmd ~reply ~sw ~clock ~inbox
    | Register_tenant { tenant; brand; push_stream; reply } ->
      let is_reregister = Map.mem state.registry tenant in
      let registry = Map.set state.registry ~key:tenant ~data:push_stream in
      Eio.Promise.resolve reply (Ok tenant);
      let state = { state with registry } in
      let state =
        match is_reregister with
        | true ->
          log "tenant %s re-registering (replacing stale connection)" tenant;
          state
        | false ->
          log "tenant %s registered (brand=%s)" tenant
            (Option.value brand ~default:"(none)");
          state
          |> fun s -> flush_pending_deliveries s tenant push_stream
          |> fun s -> update_tenant_config s tenant brand
      in
      broadcast_config state;
      state
    | Unregister_tenant { tenant } ->
      let registry = Map.remove state.registry tenant in
      log "tenant %s unregistered" tenant;
      let state = { state with registry } in
      broadcast_config state;
      state
    | Launch_timeout { tenant } ->
      (match List.Assoc.find state.starting ~equal:String.equal tenant with
       | None -> state
       | Some sentinel ->
         List.iter sentinel.pending ~f:(fun pd ->
           let msg = Printf.sprintf "tenant %s failed to start within timeout" tenant in
           Eio.Promise.resolve pd.reply (Error msg);
           log "timeout: failed to deliver URL to %s: %s" tenant pd.url);
         let starting = List.Assoc.remove state.starting ~equal:String.equal tenant in
         log "tenant %s start timed out" tenant;
         { state with starting })
  in
  coordinator_loop state inbox ~sw ~clock

(* -- Registration (long-lived connection) *)

let rec forward_pushes push_stream flow =
  let msg = Eio.Stream.take push_stream in
  Eio.Flow.copy_string (msg ^ "\n") flow;
  forward_pushes push_stream flow

let rec receive_requests tenant inbox push_stream reader =
  let line = Eio.Buf_read.line reader in
  let () = match Protocol.deserialize_request line with
    | Error msg ->
      log "req[%s]: parse error: %s" tenant msg
    | Ok req ->
      log "req[%s]: id=%d %s" tenant req.id (Protocol.name_of_command req.command);
      let (promise, reply) = Eio.Promise.create () in
      Eio.Stream.add inbox (Dispatch { id = req.id; command = Protocol.command_of_wire req.command; tenant; reply });
      let response_line = Eio.Promise.await promise in
      Eio.Stream.add push_stream response_line
  in
  receive_requests tenant inbox push_stream reader

let handle_register inbox ~tenant ~brand ~register_id flow reader =
  let push_stream = Eio.Stream.create 16 in
  let (promise, reply) = Eio.Promise.create () in
  Eio.Stream.add inbox (Register_tenant { tenant; brand; push_stream; reply });
  match Eio.Promise.await promise with
  | Error msg ->
    let err_msg = Protocol.Wire.Response { id = register_id; response = Err { message = msg } } in
    Eio.Flow.copy_string (Protocol.serialize_server_message err_msg ^ "\n") flow
  | Ok tenant_id ->
    begin
      try
        let ok_msg = Protocol.Wire.Response { id = register_id; response = Ok_registered { tenant_id } } in
        Eio.Flow.copy_string (Protocol.serialize_server_message ok_msg ^ "\n") flow;
        Eio.Fiber.both
          (fun () -> forward_pushes push_stream flow)
          (fun () -> receive_requests tenant inbox push_stream reader)
      with
      | _ -> ()
    end;
    Eio.Stream.add inbox (Unregister_tenant { tenant })

(* -- Connection handling *)

let handle_connection inbox flow =
  let reader = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) flow in
  match Eio.Buf_read.line reader with
  | exception (End_of_file | Eio.Io _) -> ()
  | line ->
    log "req: %s" line;
    match Protocol.deserialize_request line with
     | Error msg ->
       let err_msg = Protocol.Wire.Response { id = 0; response = Err { message = msg } } in
       let resp = Protocol.serialize_server_message err_msg in
       log "res: %s" resp;
       Eio.Flow.copy_string (resp ^ "\n") flow
     | Ok { id; tenant; command; } ->
       let tenant = Option.value tenant ~default:"default" in
       match Protocol.command_of_wire command with
        | Command (Register brand) ->
          log "res[%s]: registering (brand=%s)" tenant
            (Option.value brand ~default:"(none)");
          handle_register inbox ~tenant ~brand ~register_id:id flow reader
        | packed_cmd ->
          let (promise, reply) = Eio.Promise.create () in
          Eio.Stream.add inbox (Dispatch { id; command = packed_cmd; tenant; reply });
          let response_line = Eio.Promise.await promise in
          Eio.Flow.copy_string (response_line ^ "\n") flow

(* -- Main *)

let default_config_path =
  Sys.getenv_exn "HOME" ^ "/.config/alloy/config.json"

let load_and_validate_config config_path =
  let config =
    match load_config config_path with
    | Ok c -> c
    | Error msg -> failwith msg
  in
  let compiled_rules =
    match compile_rules config.rules with
    | Ok cr -> cr
    | Error msg -> failwith (Printf.sprintf "invalid rules in config: %s" msg)
  in
  (match List.is_empty config.allowed_networks with
   | true -> failwith "no valid allowed_networks configured — all connections would be rejected"
   | false -> ());
  (config, compiled_rules)

let check_connection_allowed ~allowed_networks addr =
  match addr with
  | `Tcp (ip, _port) ->
    let ip_str = Unix.string_of_inet_addr (Eio_unix.Net.Ipaddr.to_unix ip) in
    Cidr.ip_allowed ~allowed_networks ip_str
  | _ -> false

let start_listeners ~sw net listen_addrs =
  let listeners =
    List.filter_map listen_addrs ~f:(fun ({ host; port } : Protocol.listen_address) ->
      match
        let ip = Eio_unix.Net.Ipaddr.of_unix (Unix.inet_addr_of_string host) in
        let listener = Eio.Net.listen ~sw ~backlog:128 ~reuse_addr:true net (`Tcp (ip, port)) in
        log "listening on %s:%d" host port;
        listener
      with
      | listener -> Some listener
      | exception exn ->
        log "warning: failed to listen on %s:%d: %s" host port (Exn.to_string exn);
        None)
  in
  match listeners with
  | [] -> failwith "no listeners could be started"
  | _ -> listeners

let rec accept_loop ~sw ~allowed_networks inbox listener =
  Eio.Net.accept_fork ~sw listener
    ~on_error:(fun exn -> log "connection error: %s" (Exn.to_string exn))
    (fun flow addr ->
       match check_connection_allowed ~allowed_networks addr with
       | true -> handle_connection inbox flow
       | false ->
         log "rejected connection from disallowed address";
         Eio.Flow.close flow);
  accept_loop ~sw ~allowed_networks inbox listener

let run config_path =
  Stdlib.Sys.set_signal Stdlib.Sys.sigchld Stdlib.Sys.Signal_ignore;
  try
    Eio_main.run @@ fun env ->
    let clock = Eio.Stdenv.clock env in
    let net = Eio.Stdenv.net env in
    let (config, compiled_rules) = load_and_validate_config config_path in
    let initial_state =
      {
        config;
        config_path;
        compiled_rules;
        registry = Map.empty (module String);
        starting = [];
        cooldowns = [];
        start_time = Unix.gettimeofday ();
      }
    in
    let inbox = Eio.Stream.create 64 in
    Eio.Switch.run @@ fun sw ->
    let listeners = start_listeners ~sw net config.listen in
    let allowed_networks = config.allowed_networks in
    Eio.Fiber.all
      ((fun () -> coordinator_loop initial_state inbox ~sw ~clock)
       :: List.map listeners ~f:(fun l -> fun () -> accept_loop ~sw ~allowed_networks inbox l))
  with Failure msg ->
    log "fatal: %s" msg;
    Stdlib.exit 1

let () =
  let open Cmdliner in
  let config_path =
    let doc = "Path to configuration file." in
    Arg.(value & opt string (default_config_path)
         & info [ "config"; "c" ] ~docv:"PATH" ~doc)
  in
  let cmd =
    Cmd.v (Cmd.info "alloyd" ~doc:"Alloy URL routing daemon")
      Term.(const run $ config_path)
  in
  Stdlib.exit (Cmd.eval cmd)
