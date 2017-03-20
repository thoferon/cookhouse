open Sharp.Core

val project_network : (Menu.t, 'a) Signal.t -> Api.Project.t list -> string
                      -> Dom_html.element Js.t -> unit Network.t
