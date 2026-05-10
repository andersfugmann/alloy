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
  history : Protocol.history_entry list;
  history_path : string;
  exclude_patterns : string list;
  compiled_excludes : Re.re list;
}

(* Try to push a message to a tenant connection.
   Returns true on success, false if queue is full (client presumed dead). *)
let try_push (conn : tenant_connection) (msg : string) : bool =
  match Eio.Stream.length conn.push_stream < Constants.push_queue_capacity with
  | true -> Eio.Stream.add conn.push_stream msg; true
  | false -> conn.close (); false

(* -- Handler environment, coordinator messages, and existential handler type *)

type handler_env = {
  state : state;
  tenant : string;
  connection : tenant_connection;
  sw : Eio.Switch.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  inbox : coordinator_msg Eio.Stream.t;
}

and packed_handler =
  | Handler : {
      cmd : ('req, 'resp) Protocol.command;
      handle : 'req -> handler_env -> respond:(('resp, string) Result.t -> unit) -> state;
    } -> packed_handler

and coordinator_action =
  | Dispatch of {
      handler : packed_handler;
      request_json : Yojson.Safe.t;
      request_id : int;
      connection : tenant_connection;
    }
  | Unregister
  | Launch_timeout

and coordinator_msg = {
  sender : string;
  action : coordinator_action;
}

let default_config () : Protocol.config =
  {
    listen = Constants.default_listen;

    allowed_networks = Constants.default_allowed_networks;
    tenants = [];
    defaults =
      { unmatched = "local";
        cooldown_seconds = Constants.default_cooldown_seconds;
        browser_launch_timeout = Constants.default_browser_launch_timeout };
  }

let default_exclude_patterns = [
  "^https?://www\\.google\\..*/search";
  "^https?://www\\.bing\\.com/search";
  "^https?://search\\.yahoo\\.com/search";
  "^https?://duckduckgo\\.com/";
  "^https?://www\\.baidu\\.com/s";
  "^https?://yandex\\..*/search";
  "^https?://search\\.brave\\.com/search";
  "^https?://www\\.ecosia\\.org/search";
  "^https?://www\\.startpage\\.com/";
]

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

let history_path_of config_path =
  Stdlib.Filename.dirname config_path ^ "/history.json"

let excludes_path_of config_path =
  Stdlib.Filename.dirname config_path ^ "/exclude_patterns.json"

let save_excludes config_path patterns =
  let path = excludes_path_of config_path in
  mkdir_p (Stdlib.Filename.dirname path);
  let json = Protocol.exclude_patterns_to_yojson patterns in
  let content = Yojson.Safe.pretty_to_string json in
  Out_channel.write_all path ~data:(content ^ "\n")

let load_excludes config_path =
  let path = excludes_path_of config_path in
  match Stdlib.Sys.file_exists path with
  | true ->
    let content = In_channel.read_all path in
    Result.bind (Protocol.parse_json_string content) ~f:Protocol.exclude_patterns_of_yojson
  | false ->
    log "no exclude patterns found, creating default at %s" path;
    (try save_excludes config_path default_exclude_patterns
     with exn ->
       log "warning: could not write default exclude patterns: %s"
         (Exn.to_string exn));
    Ok default_exclude_patterns

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
      save_rules config_path rules
    | Error msg ->
      log "warning: could not parse rules from config: %s" msg

let migrate_excludes_from_config config_path json =
  let excludes_path = excludes_path_of config_path in
  match Stdlib.Sys.file_exists excludes_path with
  | true -> ()
  | false ->
    let excludes_json = Yojson.Safe.Util.member "history_exclude_patterns" json in
    match excludes_json with
    | `Null -> ()
    | excludes_json ->
      match Protocol.exclude_patterns_of_yojson excludes_json with
      | Ok patterns ->
        log "found history_exclude_patterns in config.json, migrating to exclude_patterns.json";
        save_excludes config_path patterns
      | Error msg ->
        log "warning: could not parse exclude patterns from config: %s" msg

let fill_config_defaults (config : Protocol.config) : Protocol.config =
  let listen =
    match config.listen with
    | [] -> Constants.default_listen
    | l -> l
  in
  let allowed_networks =
    match config.allowed_networks with
    | [] -> Constants.default_allowed_networks
    | n -> n
  in
  { config with listen; allowed_networks }

let load_config path =
  match Stdlib.Sys.file_exists path with
  | true ->
    let content = In_channel.read_all path in
    Result.bind (Protocol.parse_json_string content) ~f:(fun json ->
      migrate_rules_from_config path json;
      migrate_excludes_from_config path json;
      match Protocol.config_of_yojson json with
      | Ok config ->
        let config = fill_config_defaults config in
        (* Save back with defaults filled in and rules stripped *)
        (match
           save_config_to_path path config
         with
         | () -> ()
         | exception exn ->
           log "warning: could not update config: %s" (Exn.to_string exn));
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
    let push_str = Protocol.serialize_frame (Protocol.make_push_frame (Navigate url)) in
    (match try_push conn push_str with
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
                 Eio.Stream.add inbox { sender = target; action = Launch_timeout });
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

(* -- State helpers *)

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

let try_save_excludes state =
  try
    save_excludes state.config_path state.exclude_patterns;
    Ok ()
  with exn ->
    Error (Printf.sprintf "failed to save exclude patterns: %s" (Exn.to_string exn))

let broadcast_config (state : state) : state =
  let registered = Map.keys state.registry in
  let push = Protocol.Config_updated { config = state.config; registered_tenants = registered } in
  let s = Protocol.serialize_frame (Protocol.make_push_frame push) in
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
      let push_str = Protocol.serialize_frame (Protocol.make_push_frame (Navigate pd.url)) in
      let _delivered = try_push conn push_str in
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
        { browser_cmd = suggested_cmd; label = tenant; color = Constants.default_tenant_color; brand }
      in
      log "auto-added tenant %s to config" tenant;
      state.config.tenants @ [ (tenant, new_tenant) ]
  in
  let config = { state.config with tenants } in
  let state = { state with config } in
  let _ = try_save_config state in
  state

(* -- Handler functions *)

let handle_register params env ~respond:_ =
  let registry = Map.set env.state.registry ~key:env.tenant ~data:env.connection in
  (* Send Registered push instead of response (registration is fire-and-forget) *)
  let push_str = Protocol.serialize_frame (Protocol.make_notification_frame (Registered env.tenant)) in
  Eio.Stream.add env.connection.push_stream push_str;
  log "tenant %s registered (brand=%s)" env.tenant (Option.value params.Protocol.brand ~default:"(none)");
  { env.state with registry }
  |> fun s -> flush_pending_deliveries s env.tenant env.connection
  |> fun s -> update_tenant_config s env.tenant params.brand
  |> broadcast_config

let record_history state ~url ~title =
  let title = Option.value title ~default:"" in
  let timestamp = Unix.gettimeofday () in
  let history = History.record state.history ~url ~title ~timestamp in
  History.save state.history_path history;
  { state with history }

let handle_page_loaded (request : Protocol.page_loaded_request) env ~respond =
  let url = request.url in
  let is_excluded =
    List.exists env.state.compiled_excludes ~f:(fun re -> Re.execp re url)
  in
  match is_excluded with
  | true ->
    respond (Ok ());
    env.state
  | false ->
    let state = record_history env.state ~url ~title:request.title in
    respond (Ok ());
    state

let handle_open (request : Protocol.open_request) env ~respond =
  let state = env.state in
  let url = request.url in
  let matched_target =
    find_matching_rule state.compiled_rules url
    |> Option.value_map ~default:state.config.defaults.unmatched ~f:fst
  in
  let now = Unix.gettimeofday () in
  let (in_cooldown, pruned) = check_and_prune_cooldowns state.cooldowns ~now ~key:url in
  let state = { state with cooldowns = pruned } in
  let is_local =
    in_cooldown
    || String.equal matched_target env.tenant
    || String.equal matched_target "local"
  in
  match is_local with
  | true ->
    respond (Ok Protocol.Local);
    state
  | false ->
    let cooldown = Float.of_int state.config.defaults.cooldown_seconds in
    let cooldowns = { key = url; expires = now +. cooldown } :: state.cooldowns in
    let state = { state with cooldowns } in
    let (state, promise) = deliver_url state matched_target url ~sw:env.sw ~clock:env.clock ~inbox:env.inbox in
    Eio.Fiber.fork ~sw:env.sw (fun () ->
      let result = Eio.Promise.await promise in
      respond result);
    state

let handle_open_on (request : Protocol.open_on_request) env ~respond =
  let state = env.state in
  let (state, promise) = deliver_url state request.target request.url ~sw:env.sw ~clock:env.clock ~inbox:env.inbox in
  Eio.Fiber.fork ~sw:env.sw (fun () ->
    let result = Eio.Promise.await promise in
    respond result);
  state

let handle_test (request : Protocol.open_request) env ~respond =
  let result =
    match find_matching_rule env.state.compiled_rules request.url with
    | Some (target, idx) -> Protocol.Match { tenant = target; rule_index = idx }
    | None -> Protocol.No_match { default_tenant = env.state.config.defaults.unmatched }
  in
  respond (Ok result);
  env.state

let handle_get_config _request env ~respond =
  respond (Ok env.state.config);
  env.state

let compile_excludes patterns =
  List.filter_map patterns ~f:(fun pattern ->
    match compile_regex pattern with
    | Ok re -> Some re
    | Error msg ->
      Eio.traceln "Warning: invalid history exclude pattern '%s': %s" pattern msg;
      None)

let handle_set_config config env ~respond =
  let state = { env.state with config } in
  let resp = try_save_config state in
  respond resp;
  (match resp with
   | Ok () -> broadcast_config state
   | Error _ -> state)

let handle_get_rules _request env ~respond =
  respond (Ok env.state.rules);
  env.state

let handle_set_rules rules env ~respond =
  match compile_rules rules with
  | Error msg ->
    respond (Error (Printf.sprintf "invalid rules: %s" msg));
    env.state
  | Ok compiled_rules ->
    let state = { env.state with rules; compiled_rules } in
    let resp = try_save_rules state in
    respond resp;
    state

let handle_get_exclude_patterns _request env ~respond =
  respond (Ok env.state.exclude_patterns);
  env.state

let handle_set_exclude_patterns patterns env ~respond =
  let compiled_excludes = compile_excludes patterns in
  let state = { env.state with exclude_patterns = patterns; compiled_excludes } in
  let resp = try_save_excludes state in
  respond resp;
  state

let handle_status _request env ~respond =
  let tenants = Map.keys env.state.registry in
  let uptime = Unix.gettimeofday () -. env.state.start_time |> Float.to_int in
  respond (Ok { Protocol.registered_tenants = tenants; uptime_seconds = uptime });
  env.state

let handle_connection_info _request env ~respond =
  respond (Ok { Protocol.tenant_id = env.tenant });
  env.state

let handle_lookup (request : Protocol.lookup_request) env ~respond =
  let today = Float.to_int (Unix.gettimeofday () /. 86400.) in
  let results = History.lookup env.state.history
    ~query:request.query ~scope:request.scope ~max_results:request.max_results
    ~max_age_days:request.max_age_days ~today in
  respond (Ok results);
  env.state

let handle_import_history entries env ~respond =
  let filtered = List.filter entries ~f:(fun entry ->
    not (List.exists env.state.compiled_excludes ~f:(fun re ->
      Re.execp re entry.Protocol.url)))
  in
  let history = History.merge env.state.history filtered in
  History.save env.state.history_path history;
  respond (Ok (List.length history));
  { env.state with history }

let handle_delete_history urls env ~respond =
  let history = History.delete env.state.history ~urls in
  History.save env.state.history_path history;
  respond (Ok (List.length history));
  { env.state with history }

(* -- Command lookup: single match on string → handler bundle *)

let lookup_handler : string -> (packed_handler, string) Result.t = function
  | "register" -> Ok (Handler { cmd = Register; handle = handle_register })
  | "page_loaded" -> Ok (Handler { cmd = Page_loaded; handle = handle_page_loaded })
  | "open" -> Ok (Handler { cmd = Open; handle = handle_open })
  | "open_on" -> Ok (Handler { cmd = Open_on; handle = handle_open_on })
  | "test" -> Ok (Handler { cmd = Test; handle = handle_test })
  | "get_config" -> Ok (Handler { cmd = Get_config; handle = handle_get_config })
  | "set_config" -> Ok (Handler { cmd = Set_config; handle = handle_set_config })
  | "get_rules" -> Ok (Handler { cmd = Get_rules; handle = handle_get_rules })
  | "set_rules" -> Ok (Handler { cmd = Set_rules; handle = handle_set_rules })
  | "status" -> Ok (Handler { cmd = Status; handle = handle_status })
  | "connection_info" -> Ok (Handler { cmd = Connection_info; handle = handle_connection_info })
  | "lookup" -> Ok (Handler { cmd = Lookup; handle = handle_lookup })
  | "import_history" -> Ok (Handler { cmd = Import_history; handle = handle_import_history })
  | "delete_history" -> Ok (Handler { cmd = Delete_history; handle = handle_delete_history })
  | "get_exclude_patterns" -> Ok (Handler { cmd = Get_exclude_patterns; handle = handle_get_exclude_patterns })
  | "set_exclude_patterns" -> Ok (Handler { cmd = Set_exclude_patterns; handle = handle_set_exclude_patterns })
  | name -> Result.failf "unknown command: %s" name

(* -- Response formatting *)

let serialize_response id result =
  Protocol.make_response_frame id result
  |> Protocol.serialize_frame

(* -- Generic executor: no GADT matching *)

let execute_handler (Handler { cmd; handle }) request_json request_id env =
  match Protocol.request_deserializer cmd request_json with
  | Error msg ->
    let response = serialize_response request_id (Error msg) in
    Eio.Stream.add env.connection.push_stream response;
    env.state
  | Ok request ->
    let respond result =
      let json_result = Result.map result ~f:(Protocol.response_serializer cmd) in
      let response = serialize_response request_id json_result in
      Eio.Stream.add env.connection.push_stream response
    in
    handle request env ~respond

(* -- Coordinator loop *)

let rec coordinator_loop state inbox ~sw ~clock =
  let { sender; action } = Eio.Stream.take inbox in
  let state = match action with
    | Dispatch { handler; request_json; request_id; connection } ->
      let env = { state; tenant = sender; connection; sw; clock; inbox } in
      execute_handler handler request_json request_id env
    | Unregister ->
      let registry = Map.remove state.registry sender in
      log "tenant %s unregistered" sender;
      broadcast_config { state with registry }
    | Launch_timeout ->
      (match List.Assoc.find state.starting ~equal:String.equal sender with
       | None -> state
       | Some sentinel ->
         List.iter sentinel.pending ~f:(fun pd ->
           let msg = Printf.sprintf "tenant %s failed to start within timeout" sender in
           Eio.Promise.resolve pd.reply (Error msg);
           log "timeout: failed to deliver URL to %s: %s" sender pd.url);
         let starting = List.Assoc.remove state.starting ~equal:String.equal sender in
         log "tenant %s start timed out" sender;
         { state with starting })
  in
  coordinator_loop state inbox ~sw ~clock

(* -- Connection handling *)

let rec forward_pushes push_stream flow =
  let msg = Eio.Stream.take push_stream in
  Eio.Flow.copy_string (msg ^ "\n") flow;
  forward_pushes push_stream flow

let rec receive_requests ~tenant inbox connection reader =
  match Eio.Buf_read.line reader with
  | exception End_of_file -> tenant
  | exception Eio.Io _ -> tenant
  | line ->
    let result =
      Result.bind (Protocol.deserialize_frame line) ~f:(fun frame ->
        Protocol.request_payload_of_yojson frame.payload
        |> Result.map ~f:(fun rp -> (frame, rp)))
    in
    match result with
    | Error msg ->
      log "req[%s]: parse error: %s (raw: %s)" (Option.value tenant ~default:"?") msg line;
      receive_requests ~tenant inbox connection reader
    | Ok (frame, rp) ->
      let tenant_name =
        match tenant with
        | Some t -> t
        | None ->
          (* Derive tenant from register_request.name for the first request *)
          match String.equal rp.command "register" with
          | true ->
            begin match Protocol.request_deserializer Protocol.Register rp.params with
            | Ok req -> Option.value req.name ~default:"anonymous"
            | Error _ -> "anonymous"
            end
          | false -> "anonymous"
      in
      log "req[%s]: id=%d %s" tenant_name frame.correlation_id rp.command;
      match lookup_handler rp.command with
      | Error msg ->
        log "req[%s]: command error: %s" tenant_name msg;
        let response = serialize_response frame.correlation_id (Error msg) in
        Eio.Stream.add connection.push_stream response;
        receive_requests ~tenant inbox connection reader
      | Ok handler ->
        Eio.Stream.add inbox { sender = tenant_name; action = Dispatch { handler; request_json = rp.params; request_id = frame.correlation_id; connection } };
        let tenant =
          match String.equal rp.command "register" with
          | true -> Some tenant_name
          | false -> tenant
        in
        receive_requests ~tenant inbox connection reader

let handle_connection inbox flow =
  let push_stream = Eio.Stream.create Constants.push_queue_capacity in
  let connection = { push_stream; close = (fun () -> Eio.Flow.close flow) } in
  let tenant =
    Eio.Switch.run @@ fun sw ->
    Eio.Fiber.fork_daemon ~sw (fun () ->
      (try forward_pushes push_stream flow
       with Eio.Cancel.Cancelled _ as ex -> raise ex | _ -> ());
      `Stop_daemon);
    let reader = Eio.Buf_read.of_flow ~max_size:Constants.max_read_buffer flow in
    receive_requests ~tenant:None inbox connection reader
  in
  match tenant with
  | Some name -> Eio.Stream.add inbox { sender = name; action = Unregister }
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
        let listener = Eio.Net.listen ~sw ~backlog:Constants.tcp_listen_backlog ~reuse_addr:true net (`Tcp (ip, port)) in
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
    let exclude_patterns =
      match load_excludes config_path with
      | Ok p -> p
      | Error msg -> failwith (Printf.sprintf "failed to load exclude patterns: %s" msg)
    in
    let compiled_excludes = compile_excludes exclude_patterns in
    let history_path = history_path_of config_path in
    let history = History.load history_path in
    let history =
      let filtered = List.filter history ~f:(fun entry ->
        not (List.exists compiled_excludes ~f:(fun re -> Re.execp re entry.Protocol.url)))
      in
      match Int.equal (List.length filtered) (List.length history) with
      | true -> history
      | false ->
        Eio.traceln "Pruned %d excluded entries from history"
          (List.length history - List.length filtered);
        History.save history_path filtered;
        filtered
    in
    let initial_state =
      {
        config;
        config_path;
        rules;
        compiled_rules;
        exclude_patterns;
        compiled_excludes;
        registry = Map.empty (module String);
        starting = [];
        cooldowns = [];
        start_time = Unix.gettimeofday ();
        history;
        history_path;
      }
    in
    let inbox = Eio.Stream.create Constants.coordinator_inbox_capacity in
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
