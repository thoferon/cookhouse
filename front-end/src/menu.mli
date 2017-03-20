open Sharp.Core

type t =
  | Overview
  | Projects
  | Project of string

val menu_network : ('a, t) Signal.t -> (unit, 'b) Signal.t -> Api.Project.t list
                   -> Dom_html.element Js.t -> unit Network.t
