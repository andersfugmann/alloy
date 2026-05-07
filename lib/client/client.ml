open! Base
open! Stdio

let ( let* ) = Lwt.bind

(* -- Events exposed to the caller *)

type event =
  | Push of Protocol.push
  | Disconnected

(* -- Internal types *)

type pending_entry = Entry : {
  cmd : ('req, 'resp) Protocol.command;
  resolver : ('resp, string) Result.t Lwt.u;
} -> pending_entry

type outgoing = Outgoing : {
  cmd : ('req, 'resp) Protocol.command;
  request : 'req;
  resolver : ('resp, string) Result.t Lwt.u;
} -> outgoing

type command =
  | Typed of outgoing
  | Raw of { command : string; params : Yojson.Safe.t; resolver : Protocol.frame Lwt.u }

type subclient_entry = {
  original_id : int;
  sub_tenant : string;
}

type loop_state = {
  next_id : int;
  pending : pending_entry Map.M(Int).t;
  raw_pending : Protocol.frame Lwt.u Map.M(Int).t;
  subclient_pending : subclient_entry Map.M(Int).t;
}

(* -- Connection (opaque to caller) *)

type connection = {
  push_command : command option -> unit;
  tenant_name : string;
  subclient_write : string -> unit;
  subclient_read : string Lwt_stream.t;
}

(* -- Message parsing *)

type incoming_msg =
  | Server_push of Protocol.push
  | Response of { id : int; result : (Yojson.Safe.t, string) Result.t }
  | Parse_error of string

let parse_frame_payload (frame : Protocol.frame) : incoming_msg =
  match frame.id with
  | 0 ->
    begin match Protocol.parse_push_payload frame with
    | Ok push -> Server_push push
    | Error msg -> Parse_error (Printf.sprintf "invalid push: %s" msg)
    end
  | _ ->
    match Protocol.parse_response_payload frame with
    | Ok (Protocol.Success json) -> Response { id = frame.id; result = Ok json }
    | Ok (Protocol.Failure msg) -> Response { id = frame.id; result = Error msg }
    | Error msg -> Parse_error (Printf.sprintf "invalid response: %s" msg)

let parse_message (raw : string) : incoming_msg =
  match Protocol.deserialize_frame raw with
  | Error msg -> Parse_error (Printf.sprintf "invalid frame: %s" msg)
  | Ok frame -> parse_frame_payload frame

(* -- Connection thread *)

let dispatch_pending (state : loop_state) (id : int)
    (result : (Yojson.Safe.t, string) Result.t)
    ~(push_subclient : string option -> unit) : loop_state =
  match Map.find state.pending id with
  | Some (Entry { cmd; resolver }) ->
    let typed_result =
      match result with
      | Ok json -> Protocol.response_deserializer cmd json
      | Error msg -> Error msg
    in
    Lwt.wakeup_later resolver typed_result;
    { state with pending = Map.remove state.pending id }
  | None ->
    match Map.find state.raw_pending id with
    | Some resolver ->
      Lwt.wakeup_later resolver (Protocol.make_response_frame id result);
      { state with raw_pending = Map.remove state.raw_pending id }
    | None ->
      match Map.find state.subclient_pending id with
      | Some { original_id; sub_tenant } ->
        let frame = Protocol.make_response_frame original_id ~tenant:sub_tenant result in
        push_subclient (Some (Protocol.serialize_frame frame));
        { state with subclient_pending = Map.remove state.subclient_pending id }
      | None -> state

let handle_incoming (state : loop_state) (raw : string)
    ~(push_event : event option -> unit)
    ~(push_subclient : string option -> unit) : loop_state =
  match parse_message raw with
  | Parse_error _msg -> state
  | Server_push p ->
    push_event (Some (Push p));
    push_subclient (Some raw);
    state
  | Response { id; result } ->
    dispatch_pending state id result ~push_subclient

let handle_command (state : loop_state) (cmd : command)
    ~(write : string -> unit) : loop_state =
  let id = state.next_id in
  match cmd with
  | Typed (Outgoing { cmd = c; request; resolver }) ->
    let frame = Protocol.make_request_frame c request id None in
    write (Protocol.serialize_frame frame);
    { state with
      next_id = id + 1;
      pending = Map.set state.pending ~key:id ~data:(Entry { cmd = c; resolver }) }
  | Raw { command; params; resolver } ->
    let frame = Protocol.make_request_frame_raw ~command ~params ~id ~tenant:None in
    write (Protocol.serialize_frame frame);
    { state with
      next_id = id + 1;
      raw_pending = Map.set state.raw_pending ~key:id ~data:resolver }

let handle_subclient_request (state : loop_state) (raw : string)
    ~(write : string -> unit) : loop_state =
  match Protocol.deserialize_frame raw with
  | Error _msg -> state
  | Ok frame ->
    let id = state.next_id in
    let sub_tenant = Option.value frame.tenant ~default:"unknown" in
    let wire_frame = { Protocol.id; tenant = None; payload = frame.payload } in
    write (Protocol.serialize_frame wire_frame);
    { state with
      next_id = id + 1;
      subclient_pending = Map.set state.subclient_pending ~key:id
        ~data:{ original_id = frame.id; sub_tenant } }

let run_loop ~(command_stream : command Lwt_stream.t)
    ~(read : string Lwt_stream.t)
    ~(subclient_write_stream : string Lwt_stream.t)
    ~(push_event : event option -> unit)
    ~(push_subclient : string option -> unit)
    ~(write : string -> unit) : unit Lwt.t =
  let initial_state = {
    next_id = 1;
    pending = Map.empty (module Int);
    raw_pending = Map.empty (module Int);
    subclient_pending = Map.empty (module Int);
  } in
  let rec loop state =
    let* msg = Lwt.pick [
      (let* c = Lwt_stream.next command_stream in Lwt.return (`Command c));
      (let* raw = Lwt_stream.next read in Lwt.return (`Incoming raw));
      (let* raw = Lwt_stream.next subclient_write_stream in Lwt.return (`Subclient raw));
    ] in
    match msg with
    | `Command cmd ->
      loop (handle_command state cmd ~write)
    | `Incoming raw ->
      loop (handle_incoming state raw ~push_event ~push_subclient)
    | `Subclient raw ->
      loop (handle_subclient_request state raw ~write)
  in
  loop initial_state

(* -- Public API *)

let tenant_name (conn : connection) : string = conn.tenant_name

let init ~(write : string -> unit) ~(read : string Lwt_stream.t)
    ?tenant ?name ?brand () : (connection * event Lwt_stream.t) Lwt.t =
  let (command_stream, push_command) = Lwt_stream.create () in
  let (event_stream, push_event) = Lwt_stream.create () in
  let (subclient_write_stream, push_subclient_write) = Lwt_stream.create () in
  let (subclient_read, push_subclient) = Lwt_stream.create () in
  let subclient_write raw = push_subclient_write (Some raw) in
  (* Send registration as id=0, fire-and-forget *)
  let register_req : Protocol.register_request = { brand; address = None; name } in
  let frame = Protocol.make_request_frame Register register_req 0 tenant in
  write (Protocol.serialize_frame frame);
  (* Wait for Registered push to learn assigned tenant name *)
  let rec await_registered () =
    let* raw = Lwt_stream.next read in
    match parse_message raw with
    | Server_push (Registered { tenant_id }) ->
      Lwt.return tenant_id
    | Server_push p ->
      push_event (Some (Push p));
      await_registered ()
    | Response _ | Parse_error _ ->
      await_registered ()
  in
  let* assigned_tenant = await_registered () in
  let conn = { push_command; tenant_name = assigned_tenant;
               subclient_write; subclient_read } in
  (* Start connection thread *)
  Lwt.async (fun () ->
    Lwt.catch
      (fun () -> run_loop ~command_stream ~read ~subclient_write_stream
                   ~push_event ~push_subclient ~write)
      (fun _exn ->
        push_event (Some Disconnected);
        Lwt.return_unit));
  Lwt.return (conn, event_stream)

let call : type req resp. connection -> (req, resp) Protocol.command -> req ->
    (resp, string) Result.t Lwt.t =
  fun conn cmd request ->
    let (promise, resolver) = Lwt.wait () in
    conn.push_command (Some (Typed (Outgoing { cmd; request; resolver })));
    promise

let subclient_write (conn : connection) (raw : string) : unit =
  conn.subclient_write raw

let subclient_read (conn : connection) : string Lwt_stream.t =
  conn.subclient_read

let send_raw_command (conn : connection) ~(command : string)
    ~(params : Yojson.Safe.t) : Protocol.frame Lwt.t =
  let (promise, resolver) = Lwt.wait () in
  conn.push_command (Some (Raw { command; params; resolver }));
  promise
