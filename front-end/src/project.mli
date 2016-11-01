open Sharp.Core

val project_network : ('a, Menu.t) Behaviour.t -> Api.Project.t list -> string
                      -> Dom_html.element Js.t -> unit Network.t
