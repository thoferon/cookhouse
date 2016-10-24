module Token : sig
  val get : unit -> string option
  val set : string -> unit
  val clear : unit -> unit
end

val push_state : string -> string -> unit

exception Element_not_found of string

(** Get an element of the DOM or raise Element_not_found if the element does not
    exist *)
val get_element    :                          string -> Dom_html.element Js.t
val get_element_in : Dom_html.element Js.t -> string -> Dom_html.element Js.t

val add_class : Dom_html.element Js.t -> string -> unit
val remove_class : Dom_html.element Js.t -> string -> unit
