open Yojson.Basic
open Yojson.Basic.Util

open XmlHttpRequest

open Utils

type t =
  { identifier      : string
  ; source_location : string
  ; source_plugin   : string
  ; dependencies    : string list
  }

let identifier { identifier } = identifier
let source_location { source_location } = source_location
let source_plugin { source_plugin } = source_plugin
let dependencies { dependencies } = dependencies

let parse_project obj =
  let identifier = obj |> member "identifier" |> to_string
  and source_location =
    obj |> member "source" |> member "location" |> to_string
  and source_plugin =
    obj |> member "source" |> member "plugin" |> to_string
  and dependencies =
    List.map to_string (obj |> member "dependencies" |> to_list)
  in
  { identifier; source_location; source_plugin; dependencies }

let get_projects () =
  let extract { code; content } =
    match code with
    | 200 ->
       begin
         try
           let subs = from_string content |> to_list in
           let projects = List.map parse_project subs in
           Lwt.return projects
         with | e -> Lwt.fail e
       end
    | 401 -> hard_signout ()
    | _ -> Lwt.fail_with "Unknown error."
  in Lwt.bind (perform_raw_url ~headers:(api_headers ())
                               "/api/projects") extract

let build_project identifier =
  let extract { code; content } =
    match code with
    | 201 -> Lwt.return ()
    | 401 -> hard_signout ()
    | _ -> Lwt.fail_with "Unknown error."
  in
  let path = "/api/projects/" ^ identifier ^ "/build" in
  Lwt.bind (perform_raw_url ~headers:(api_headers ())
                            ~override_method:`POST
                            path) extract
