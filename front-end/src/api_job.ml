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
  | _ -> raise (Type_error ("Invalid job status ", obj))

let parse_job_json obj =
  let identifier   = string_of_int (obj |> member "id" |> to_int)
  and data         = obj |> member "data" in
  let typ          = data |> member "type" |> parse_job_type
  and status       = data |> member "status" |> parse_job_status
  and project_id   = data |> member "project_identifier" |> to_string
  and dependencies =
    List.map (fun x -> string_of_int (to_int x))
             (data |> member "dependencies" |> to_list)
  in
  { identifier; typ; status; project_id; dependencies }

let extract { code; content } =
  match code with
  | 200 ->
     begin
       try
         let obj  = from_string content in
         let subs = obj |> member "jobs" |> to_list in
         let jobs = List.map parse_job_json subs in
         Lwt.return jobs
       with | e -> Lwt.fail e
     end
  | 401 -> Lwt.fail_with "Access denied."
  | _ -> print_endline "failed"; Lwt.fail_with "Unknown error."

let get_pending_jobs () =
  Lwt.bind (perform_raw_url ~headers:(SessionInfo.http_headers ())
                            "/api/pending_jobs") extract

let get_project_jobs project_id =
  let path = "/api/projects/" ^ project_id ^ "/jobs" in
  Lwt.bind (perform_raw_url ~headers:(SessionInfo.http_headers ()) path) extract
