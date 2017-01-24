open Yojson.Basic
open Yojson.Basic.Util

open XmlHttpRequest

open Utils

type typ = Build | PostBuild

type status =
  | InQueue
  | InProgress
  | Success
  | Failure
  | Rollbacked
  | Aborted

type t =
  { identifier   : string
  ; typ          : typ
  ; status       : status
  ; project_id   : string
  ; dependencies : string list
  }

let identifier { identifier } = identifier
let typ { typ } = typ
let status { status } = status
let project_id { project_id } = project_id
let dependencies { dependencies } = dependencies

let parse_job_type obj =
  match obj |> to_string with
  | "build"      -> Build
  | "post-build" -> PostBuild
  | _ -> raise (Type_error ("Invalid job type ", obj))

let parse_job_status obj =
  match obj |> to_string with
  | "in-queue"    -> InQueue
  | "in-progress" -> InProgress
  | "success"     -> Success
  | "failure"     -> Failure
  | "rollbacked"  -> Rollbacked
  | "aborted"     -> Aborted
  | _ -> raise (Type_error ("Invalid job status ", obj))

let parse_job obj =
  try
    let identifier   = obj |> member "id" |> to_int |> string_of_int
    and typ          = obj |> member "type"               |> parse_job_type
    and status       = obj |> member "status"             |> parse_job_status
    and project_id   = obj |> member "project_identifier" |> to_string
    and dependencies =
      List.map (fun x -> string_of_int (to_int x))
               (obj |> member "dependencies" |> to_list)
    in
    { identifier; typ; status; project_id; dependencies }
  with | e -> prerr_endline "Can't parse job"; raise e

let extract_jobs { code; content } =
  match code with
  | 200 ->
     let subs = from_string content |> to_list in
     let jobs = List.map parse_job subs in
     Lwt.return jobs
  | 401 -> Lwt.fail_with "Access denied."
  | _ -> Lwt.fail_with "Unknown error."

let get_pending_jobs () =
  Lwt.bind (perform_raw_url ~headers:(api_headers ())
                            "/api/jobs/pending") extract_jobs

let get_project_jobs project_id =
  let path = "/api/projects/" ^ project_id ^ "/jobs" in
  Lwt.bind (perform_raw_url ~headers:(api_headers ()) path)
           extract_jobs

let get_job job_id =
  let extract { code; content } =
    match code with
    | 200 ->
       let obj  = from_string content in
       let job  = obj |> member "job" |> parse_job
       and deps =
         List.map parse_job (obj |> member "dependencies" |> to_list)
       and results = List.map Api_job_result.parse_job_result
                              (obj |> member "results" |> to_list)
       in
       Lwt.return (job, deps, results)
    | 401 -> Lwt.fail_with "Access denied."
    | _ -> Lwt.fail_with "Unknown error."
  in
  let path = "/api/jobs/" ^ job_id in
  Lwt.bind (perform_raw_url ~headers:(api_headers ()) path) extract
