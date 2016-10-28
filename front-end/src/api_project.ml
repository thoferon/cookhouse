open Yojson.Basic
open Yojson.Basic.Util

open XmlHttpRequest

open Utils

type t =
  { identifier : string
  }

let identifier { identifier } = identifier

let parse_project_json obj =
  let identifier = obj |> member "identifier" |> to_string in
  { identifier }

let get_projects () =
  let extract { code; content } =
    match code with
    | 200 ->
       begin
         try
           let obj = from_string content in
           let subs = obj |> member "projects" |> to_list in
           let projects = List.map parse_project_json subs in
           Lwt.return projects
         with | e -> Lwt.fail e
       end
    | 401 -> Lwt.fail_with "Access denied."
    | _ -> Lwt.fail_with "Unknown error."
  in Lwt.bind (perform_raw_url ~headers:(SessionInfo.http_headers ())
                               "/api/projects") extract

let build_project identifier =
  let extract { code; content } =
    match code with
    | 201 -> Lwt.return ()
    | 401 -> Lwt.fail_with "Access denied."
    | _ -> Lwt.fail_with "Unknown error."
  in
  let path = "/api/projects/" ^ identifier ^ "/build" in
  Lwt.bind (perform_raw_url ~headers:(SessionInfo.http_headers ())
                            ~override_method:`POST
                            path) extract
