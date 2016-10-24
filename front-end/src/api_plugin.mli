type t =
  { trigger_plugins        : string list
  ; source_plugins         : string list
  ; step_plugins           : string list
  ; authentication_plugins : (string * string) list
  }

val get_plugins : unit -> t Lwt.t
