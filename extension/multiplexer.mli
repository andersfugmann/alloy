type t
val init: unit -> t
val start : t -> Client.t -> unit
val create_client : unit -> (Client.t * Protocol.push Lwt_stream.t) Lwt.t
