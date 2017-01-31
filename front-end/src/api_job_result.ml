open Yojson.Basic
open Yojson.Basic.Util

open XmlHttpRequest

open Utils

type phase = Run | Rollback

type t =
  { identifier : string
  ; phase      : phase
  ; error      : string option
  ; start_time : float
  ; end_time   : float option
  }

let identifier { identifier } = identifier
let phase { phase } = phase
let error { error } = error
let start_time { start_time } = start_time
let end_time { end_time } = end_time

let parse_job_result_phase obj =
  match obj |> to_string with
  | "run"      -> Run
  | "rollback" -> Rollback
  | _ -> raise (Type_error ("Invalid job result phase ", obj))

let parse_job_result obj =
  try
    let identifier = obj |> member "id"         |> to_int |> string_of_int
    and phase      = obj |> member "phase"      |> parse_job_result_phase
    and error      = obj |> member "error"      |> to_option to_string
    and start_str  = obj |> member "start_time" |> to_string
    and end_str    = obj |> member "end_time"   |> to_option to_string
    in
    let start_time = Js.date##parse (Js.string start_str)
    and end_time = match end_str with
      | None -> None
      | Some str -> Some (Js.date##parse (Js.string str))
    in
    { identifier; phase; error; start_time; end_time }
  with | e -> prerr_endline "Can't parse job result"; raise e

let get_job_result_output job_result_id offset =
  let extract { code; content } =
    match code with
    | 200 -> Lwt.return content
    | 401 -> hard_signout ()
    | _   -> Lwt.fail_with "Unknown error."
  in
  let path = "/api/job_results/" ^ job_result_id ^ "/output" in
  Lwt.bind (perform_raw_url ~headers:(SessionInfo.http_headers ())
                            ~get_args:[("offset", string_of_int(offset))]
                            path) extract
