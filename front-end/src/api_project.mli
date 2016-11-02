type t

val identifier : t -> string
val source_location : t -> string
val source_plugin : t -> string
val dependencies : t -> string list

val get_projects : unit -> t list Lwt.t
val build_project : string -> unit Lwt.t
