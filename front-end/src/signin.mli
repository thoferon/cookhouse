open Sharp.Core

val signin_network : ('a, string option) Behaviour.t
                     -> Dom_html.element Js.t -> unit Network.t
