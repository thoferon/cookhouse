type t

val identifier : t -> string

val get_projects : unit -> t list Lwt.t
val build_project : string -> unit Lwt.t
