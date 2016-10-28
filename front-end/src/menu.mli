open Sharp.Core

type t =
  | Overview
  | Projects
  | Project of string

val menu_network : (t, 'a) Behaviour.t -> Dom_html.element Js.t
                   -> unit Network.t
