open Yojson.Basic

module SessionInfo : sig
  val get : unit -> (string * string) option
  val set : (string * string) -> unit
  val clear : unit -> unit
  val http_headers : unit -> (string * string) list
end

val api_headers : unit -> (string * string) list
val hard_signout : unit -> 'a

val push_state : string -> string -> unit

exception Element_not_found of string

(** Get an element of the DOM or raise Element_not_found if the element does not
    exist *)
val get_element    :                          string -> Dom_html.element Js.t
val get_element_in : Dom_html.element Js.t -> string -> Dom_html.element Js.t

val add_class : Dom_html.element Js.t -> string -> unit
val remove_class : Dom_html.element Js.t -> string -> unit

val human_readable_date : float -> string
