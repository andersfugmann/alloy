open! Base
open! Stdio

let ( let* ) = Lwt.bind

type t = {
  ports : Chrome_api.port Map.M(String).t ref;
  counter : int ref;
}

let create () : t = {
  ports = ref (Map.empty (module String));
  counter = ref 0;
}

let register_port (mux : t) (desired : string) (port : Chrome_api.port) : string =
  let rec find_unique name n =
    match Map.mem !(mux.ports) name with
    | false -> name
    | true -> find_unique (Printf.sprintf "%s_%d" desired (n + 1)) (n + 1)
  in
  let tenant_id = find_unique desired (!(mux.counter) + 1) in
  mux.ports := Map.set !(mux.ports) ~key:tenant_id ~data:port;
  mux.counter := !(mux.counter) + 1;
  tenant_id

let deregister_port (mux : t) (tenant_id : string) : unit =
  mux.ports := Map.remove !(mux.ports) tenant_id

let handle_register (mux : t) (frame : Protocol.frame) (port : Chrome_api.port) : string =
  let desired =
    match String.is_empty frame.tenant with
    | true -> "anonymous"
    | false -> frame.tenant
  in
  let tenant_id = register_port mux desired port in
  let registered_frame = Protocol.make_push_frame (Registered { tenant_id }) in
  Chrome_api.Port.post_message_json port (Protocol.serialize_frame registered_frame);
  tenant_id

let is_register_frame (frame : Protocol.frame) : bool =
  match Protocol.parse_request_payload frame with
  | Ok { command; _ } -> String.equal command "register"
  | Error _ -> false

let start (mux : t) (conn : Client.connection) : unit =
  (* Listen for incoming port connections *)
  Chrome_api.Runtime.on_connect (fun port ->
    let tenant_id = ref None in
    Chrome_api.Port.on_message_json port (fun raw ->
      match Protocol.deserialize_frame raw with
      | Error _msg -> ()
      | Ok frame ->
        begin match is_register_frame frame with
        | true ->
          tenant_id := Some (handle_register mux frame port)
        | false ->
          Client.subclient_write conn raw
        end);
    Chrome_api.Port.on_disconnect port (fun () ->
      Option.iter !tenant_id ~f:(fun tid -> deregister_port mux tid)));
  (* Route subclient_read to ports.
     Note: if a port disconnects between message arrival and lookup,
     the message is silently dropped — this is expected behavior. *)
  Lwt.async (fun () ->
    let stream = Client.subclient_read conn in
    let rec forward () =
      let* raw = Lwt_stream.next stream in
      begin match Protocol.deserialize_frame raw with
      | Error _msg -> ()
      | Ok frame ->
        begin match frame.id with
        | 0 ->
          Map.iter !(mux.ports) ~f:(fun port ->
            Chrome_api.Port.post_message_json port raw)
        | _ ->
          begin match String.is_empty frame.tenant with
          | true -> ()
          | false ->
            Option.iter (Map.find !(mux.ports) frame.tenant) ~f:(fun port ->
              Chrome_api.Port.post_message_json port raw)
          end
        end
      end;
      forward ()
    in
    Lwt.catch forward (fun _exn -> Lwt.return_unit))
