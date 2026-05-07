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

let resolve_tenant (mux : t) (desired : string) : string =
  match Map.mem !(mux.ports) desired with
  | false -> desired
  | true ->
    let rec find n =
      let candidate = Printf.sprintf "%s_%d" desired n in
      match Map.mem !(mux.ports) candidate with
      | false -> candidate
      | true -> find (n + 1)
    in
    find (!(mux.counter) + 1)

let register_port (mux : t) (tenant_id : string) (port : Chrome_api.port) : unit =
  mux.ports := Map.set !(mux.ports) ~key:tenant_id ~data:port;
  mux.counter := !(mux.counter) + 1

let deregister_port (mux : t) (tenant_id : string) : unit =
  mux.ports := Map.remove !(mux.ports) tenant_id

let handle_register (mux : t) (frame : Protocol.frame) (port : Chrome_api.port) : string =
  let desired = Option.value frame.tenant ~default:"anonymous" in
  let tenant_id = resolve_tenant mux desired in
  register_port mux tenant_id port;
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
  (* Route subclient_read to ports *)
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
          begin match frame.tenant with
          | Some tid ->
            Option.iter (Map.find !(mux.ports) tid) ~f:(fun port ->
              Chrome_api.Port.post_message_json port raw)
          | None -> ()
          end
        end
      end;
      forward ()
    in
    Lwt.catch forward (fun _exn -> Lwt.return_unit))
