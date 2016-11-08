open Sharp.Core

val session_network : unit
                      -> ((bool, (string * string) option) Behaviour.t
                          * (unit option, unit) Behaviour.t) Network.t

val signin_network : ('a, (string * string) option) Behaviour.t
                     -> Dom_html.element Js.t -> unit Network.t
