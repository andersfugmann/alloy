(** Typed OCaml bindings for Chrome extension APIs.
    All [Js.Unsafe] usage in the project is confined to [chrome_api.ml]. *)

type port

val log : string -> unit
val set_timeout : (unit -> unit) -> int -> unit
val performance_now : unit -> float

module Port : sig
  val post_message_json : port -> string -> unit
  val on_message_json : port -> (string -> unit) -> unit
  val on_disconnect : port -> (unit -> unit) -> unit
end

module Runtime : sig
  val connect_native : string -> port
  val get_url : string -> string
  val on_installed : (unit -> unit) -> unit
  val on_startup : (unit -> unit) -> unit
  val connect : unit -> port
  val on_connect : (port -> unit) -> unit
end

module Tabs : sig
  val create_url : string -> unit
  val remove : int -> unit
  val query_active : on_result:(string -> int -> unit) -> unit
  val get_title : int -> on_result:(string option -> unit) -> unit
end

module Action : sig
  val set_icon : string -> string -> string -> unit
end

module Windows : sig
  val create_popup : url:string -> width:int -> height:int -> unit
end

module Storage : sig
  val get_local :
    string list -> on_result:((string * string) list -> unit) -> unit
  val set_local :
    (string * string) list -> on_done:(unit -> unit) -> unit
end

module Context_menus : sig
  val create : id:string -> title:string -> contexts:string list -> unit
  val create_child :
    id:string ->
    parent_id:string ->
    title:string ->
    contexts:string list ->
    ?enabled:bool ->
    unit ->
    unit
  val remove_all : (unit -> unit) -> unit
  val on_clicked : (string -> string -> string -> int option -> unit) -> unit
end

module Web_navigation : sig
  val on_before_navigate : (string -> int -> int -> unit) -> unit
end

module Navigator : sig
  val get_browser_brand : unit -> string
end
