type t

type typ = Build | PostBuild

type status =
  | InQueue
  | InProgress
  | Success
  | Failure
  | Rollbacked

val identifier : t -> string
val typ : t -> typ
val status : t -> status
val project_id : t -> string
val dependencies : t -> string list

val get_pending_jobs : unit   -> t list Lwt.t
val get_project_jobs : string -> t list Lwt.t
val get_job          : string -> (t * t list * Api_job_result.t list) Lwt.t
