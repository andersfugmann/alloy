val make_client :
  ?name:string ->
  ?brand:string ->
  host:string ->
  port:int ->
  debug:bool ->
  unit ->
  (Client.t * Protocol.push Lwt_stream.t) Lwt.t
