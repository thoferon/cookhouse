open Sharp.Core

val session_network : unit
                      -> (((string * string) option, bool) Signal.t
                          * (unit, unit option) Signal.t) Network.t

val signin_network : ((string * string) option, 'a) Signal.t
                     -> Dom_html.element Js.t -> unit Network.t
