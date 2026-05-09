type t

val init :
  recv_s:Protocol.frame Lwt_stream.t ->
  send_f:(Protocol.frame -> unit) ->
  ?name:string ->
  ?brand:string -> unit -> (t * Protocol.push Lwt_stream.t) Lwt.t

val close : t -> unit
val call : t -> ('req, 'resp) Protocol.command -> 'req -> ('resp, string) Result.t Lwt.t
val proxy : t -> Protocol.json -> (Protocol.json option -> unit) -> unit
val register_broadcast : t -> (Protocol.push option -> unit) -> unit
val name : t -> string
val make_proxy_client : t -> (t * Protocol.push Lwt_stream.t) Lwt.t
