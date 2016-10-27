open Sharp.Core

val session_network : #Dom_html.eventTarget Js.t
                      -> (bool, (string * string) option) Behaviour.t Network.t

val signin_network : ('a, (string * string) option) Behaviour.t
                     -> Dom_html.element Js.t -> unit Network.t
