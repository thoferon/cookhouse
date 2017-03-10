open Yojson.Basic
open Yojson.Basic.Util

open XmlHttpRequest

open Utils

let extract_url path { code; content = token } = match code with
  | 200 ->
     begin
       try
         let path' = "/api/artefacts/" ^ token ^ "/" ^ path
         in Lwt.return path'
       with | e -> Lwt.fail e
     end
  | 401 -> hard_signout ()
  | _ -> Lwt.fail_with "Unknown error."

let get_artefact_path_for_project identifier artefact_path =
  let path    = "/api/projects/" ^ identifier ^ "/artefact_token"
  and headers = ("Accept", "text/plain") :: SessionInfo.http_headers ()
  in Lwt.bind (perform_raw_url ~headers ~override_method:`POST path)
              (extract_url artefact_path)

let get_artefact_path_for_job job_id artefact_path =
  let path = "/api/jobs/" ^ job_id ^ "/artefact_token"
  and headers = ("Accept", "text/plain") :: SessionInfo.http_headers ()
  in Lwt.bind (perform_raw_url ~headers ~override_method:`POST path)
              (extract_url artefact_path)
