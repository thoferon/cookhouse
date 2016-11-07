open Yojson.Basic

type t
type phase = Run | Rollback

val identifier : t -> string
val phase : t -> phase
val error : t -> string option

val parse_job_result : string -> json -> t

val get_job_result_output : string -> int -> string Lwt.t
