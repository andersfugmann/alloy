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

let push_queue_capacity = 16

type tenant_connection = {
  push_stream : string Eio.Stream.t;
  close : unit -> unit;
}

type state = {
  config : Protocol.config;
  config_path : string;
  rules : Protocol.rule list;
  compiled_rules : compiled_rule list;
  registry : tenant_connection Map.M(String).t;
  starting : (string * starting_tenant) list;
  cooldowns : cooldown_entry list;
  start_time : float;
}

(* Try to push a message to a tenant connection.
   Returns true on success, false if queue is full (client presumed dead). *)
let try_push (conn : tenant_connection) (msg : string) : bool =
  match Eio.Stream.length conn.push_stream < push_queue_capacity with
  | true -> Eio.Stream.add conn.push_stream msg; true
  | false -> conn.close (); false

(* -- Coordinator messages *)

type coordinator_action =
  | Dispatch of {
      request : Protocol.packed_request;
      reply : (Yojson.Safe.t, string) Result.t Eio.Promise.u;
      connection : tenant_connection;
    }
  | Unregister
  | Launch_timeout

type coordinator_msg = {
  tenant : string;
  action : coordinator_action;
}

let default_config () : Protocol.config =
  {
    listen = Constants.default_listen;

    allowed_networks = Constants.default_allowed_networks;
    tenants = [];
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

let rules_path_of config_path =
  Stdlib.Filename.dirname config_path ^ "/rules.json"

let save_rules config_path rules =
  let path = rules_path_of config_path in
  mkdir_p (Stdlib.Filename.dirname path);
  let json = Protocol.rules_to_yojson rules in
  let content = Yojson.Safe.pretty_to_string json in
  Out_channel.write_all path ~data:(content ^ "\n")

let load_rules config_path =
  let path = rules_path_of config_path in
  match Stdlib.Sys.file_exists path with
  | true ->
    let content = In_channel.read_all path in
    Result.bind (Protocol.parse_json_string content) ~f:Protocol.rules_of_yojson
  | false -> Ok []

let migrate_rules_from_config config_path json =
  let rules_json = Yojson.Safe.Util.member "rules" json in
  match rules_json with
  | `Null -> ()
  | rules_json ->
    match Protocol.rules_of_yojson rules_json with
    | Ok rules ->
      log "found rules in config.json, migrating to rules.json";
      save_rules config_path rules;
      let stripped =
        match json with
        | `Assoc fields ->
          `Assoc (List.filter fields ~f:(fun (k, _) -> not (String.equal k "rules")))
        | other -> other
      in
      let content = Yojson.Safe.pretty_to_string stripped in
      Out_channel.write_all config_path ~data:(content ^ "\n")
    | Error msg ->
      log "warning: could not parse rules from config: %s" msg

let load_config path =
  match Stdlib.Sys.file_exists path with
  | true ->
    let content = In_channel.read_all path in
    Result.bind (Protocol.parse_json_string content) ~f:(fun json ->
      match Protocol.config_of_yojson json with
      | Ok config ->
        migrate_rules_from_config path json;
        Ok config
      | Error msg -> Error msg)
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
  | Some conn ->
    let push_msg = Protocol.Push { id = 0; push = Navigate { url } } in
    (match try_push conn (Protocol.serialize_server_message push_msg) with
     | true ->
       (state, Eio.Promise.create_resolved (Ok (Protocol.Remote target)))
     | false ->
       log "deliver: tenant %s push queue full, disconnecting" target;
       let registry = Map.remove state.registry target in
       let state = { state with registry } in
       let msg = Printf.sprintf "tenant %s connection stale" target in
       (state, Eio.Promise.create_resolved (Error msg)))
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
                 Eio.Stream.add inbox { tenant = target; action = Launch_timeout });
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

let try_save_rules state =
  try
    save_rules state.config_path state.rules;
    Ok ()
  with exn ->
    Error (Printf.sprintf "failed to save rules: %s" (Exn.to_string exn))

let handle_set_config state (cfg : Protocol.config) =
  let state = { state with config = cfg } in
  (state, try_save_config state)

let handle_set_rules state (rules : Protocol.rule list) =
  match compile_rules rules with
  | Error msg -> (state, Error (Printf.sprintf "invalid rules: %s" msg))
  | Ok compiled_rules ->
    let state = { state with rules; compiled_rules } in
    (state, try_save_rules state)

(* -- Command dispatch *)

let broadcast_config (state : state) : state =
  let registered = Map.keys state.registry in
  let push = Protocol.Config_updated { config = state.config; registered_tenants = registered } in
  let msg = Protocol.Push { id = 0; push } in
  let s = Protocol.serialize_server_message msg in
  let registry =
    Map.filter state.registry ~f:(fun conn ->
      match try_push conn s with
      | true -> true
      | false ->
        log "broadcast: push queue full, disconnecting stale client";
        false)
  in
  { state with registry }

let flush_pending_deliveries state tenant conn =
  match List.Assoc.find state.starting ~equal:String.equal tenant with
  | None -> state
  | Some sentinel ->
    List.iter sentinel.pending ~f:(fun pd ->
      let push_msg = Protocol.Push { id = 0; push = Navigate { url = pd.url } } in
      let _delivered = try_push conn (Protocol.serialize_server_message push_msg) in
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

let dispatch_command state ~tenant (Protocol.Request (cmd, params, resp_to_json))
    ~reply ~connection ~sw ~clock ~inbox =
  let resolve result =
    Eio.Promise.resolve reply (Result.map result ~f:resp_to_json)
  in
  match cmd with
  | Protocol.Register ->
    let is_reregister = Map.mem state.registry tenant in
    let registry = Map.set state.registry ~key:tenant ~data:connection in
    resolve (Ok tenant);
    (match is_reregister with
     | true -> log "tenant %s re-registered (replacing stale connection)" tenant
     | false -> log "tenant %s registered (brand=%s)" tenant
                  (Option.value params.brand ~default:"(none)"));
    { state with registry }
    |> fun s -> flush_pending_deliveries s tenant connection
    |> fun s -> update_tenant_config s tenant params.brand
    |> broadcast_config
  | Protocol.Open ->
    let (state, promise) = handle_open state tenant params.url ~sw ~clock ~inbox in
    Eio.Fiber.fork ~sw (fun () ->
      let result = Eio.Promise.await promise in
      resolve result);
    state
  | Protocol.Open_on ->
    let (state, promise) = handle_open_on state params.target params.url ~sw ~clock ~inbox in
    Eio.Fiber.fork ~sw (fun () ->
      let result = Eio.Promise.await promise in
      resolve result);
    state
  | Protocol.Test ->
    let (state, resp) = handle_test state params.url in
    resolve resp;
    state
  | Protocol.Get_config ->
    resolve (Ok state.config);
    state
  | Protocol.Set_config ->
    let (state, resp) = handle_set_config state params in
    resolve resp;
    (match resp with
     | Ok () -> broadcast_config state
     | Error _ -> state)
  | Protocol.Get_rules ->
    resolve (Ok state.rules);
    state
  | Protocol.Set_rules ->
    let (state, resp) = handle_set_rules state params in
    resolve resp;
    state
  | Protocol.Status ->
    let (state, resp) = handle_status state in
    resolve resp;
    state

(* -- Coordinator loop *)

let rec coordinator_loop state inbox ~sw ~clock =
  let { tenant; action } = Eio.Stream.take inbox in
  let state = match action with
    | Dispatch { request; reply; connection } ->
      dispatch_command state ~tenant request ~reply ~connection ~sw ~clock ~inbox
    | Unregister ->
      let registry = Map.remove state.registry tenant in
      log "tenant %s unregistered" tenant;
      broadcast_config { state with registry }
    | Launch_timeout ->
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

(* -- Connection handling *)

let rec forward_pushes push_stream flow =
  let msg = Eio.Stream.take push_stream in
  Eio.Flow.copy_string (msg ^ "\n") flow;
  forward_pushes push_stream flow

let serialize_response id result =
  Protocol.Response (Protocol.make_response_envelope id result)
  |> Protocol.serialize_server_message

let rec receive_requests ~tenant inbox connection reader =
  match Eio.Buf_read.line reader with
  | exception End_of_file -> tenant
  | exception Eio.Io _ -> tenant
  | line ->
    match Protocol.deserialize_request_envelope line with
    | Error msg ->
      log "req[%s]: parse error: %s" (Option.value tenant ~default:"?") msg;
      receive_requests ~tenant inbox connection reader
    | Ok env ->
      let tenant_name = Option.value (Option.first_some env.tenant tenant) ~default:"anonymous" in
      log "req[%s]: id=%d %s" tenant_name env.id env.command;
      match Protocol.deserialize_request env.command env.params with
      | Error msg ->
        log "req[%s]: command error: %s" tenant_name msg;
        let msg = serialize_response env.id (Error msg) in
        Eio.Stream.add connection.push_stream msg;
        receive_requests ~tenant inbox connection reader
      | Ok request ->
        let (promise, reply) = Eio.Promise.create () in
        Eio.Stream.add inbox { tenant = tenant_name; action = Dispatch { request; reply; connection } };
        let result = Eio.Promise.await promise in
        let msg = serialize_response env.id result in
        log "res[%s]: %s" tenant_name msg;
        Eio.Stream.add connection.push_stream msg;
        let tenant =
          match result with
          | Ok _ ->
            (match String.equal env.command "register" with
             | true -> Some tenant_name
             | false -> tenant)
          | Error _ -> tenant
        in
        receive_requests ~tenant inbox connection reader

let handle_connection inbox flow =
  let push_stream = Eio.Stream.create push_queue_capacity in
  let connection = { push_stream; close = (fun () -> Eio.Flow.close flow) } in
  let tenant =
    Eio.Switch.run @@ fun sw ->
    Eio.Fiber.fork_daemon ~sw (fun () ->
      (try forward_pushes push_stream flow
       with Eio.Cancel.Cancelled _ as ex -> raise ex | _ -> ());
      `Stop_daemon);
    let reader = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) flow in
    receive_requests ~tenant:None inbox connection reader
  in
  match tenant with
  | Some name -> Eio.Stream.add inbox { tenant = name; action = Unregister }
  | None -> ()

(* -- Main *)

let default_config_path =
  Sys.getenv_exn "HOME" ^ "/.config/alloy/config.json"

let load_and_validate_config config_path =
  let config =
    match load_config config_path with
    | Ok c -> c
    | Error msg -> failwith msg
  in
  let rules =
    match load_rules config_path with
    | Ok r -> r
    | Error msg -> failwith (Printf.sprintf "failed to load rules: %s" msg)
  in
  let compiled_rules =
    match compile_rules rules with
    | Ok cr -> cr
    | Error msg -> failwith (Printf.sprintf "invalid rules in config: %s" msg)
  in
  (match List.is_empty config.allowed_networks with
   | true -> failwith "no valid allowed_networks configured — all connections would be rejected"
   | false -> ());
  (config, rules, compiled_rules)

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
    let (config, rules, compiled_rules) = load_and_validate_config config_path in
    let initial_state =
      {
        config;
        config_path;
        rules;
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
