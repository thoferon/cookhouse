open Sharp.Core

val job_minimal_view : Api.Job.t -> Sharp.Vdom.t

val job_network : (Menu.t, 'a) Signal.t -> Api.Project.t list -> string
                  -> string -> Dom_html.element Js.t -> unit Network.t
