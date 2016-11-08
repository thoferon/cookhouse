open Sharp.Core

type t =
  | Overview
  | Projects
  | Project of string

val menu_network : (t, 'a) Behaviour.t -> ('b, unit) Behaviour.t
                   -> Api.Project.t list -> Dom_html.element Js.t
                   -> unit Network.t
