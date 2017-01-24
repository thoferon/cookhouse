open Yojson.Basic
open Yojson.Basic.Util

open XmlHttpRequest

open Utils

type t =
  { trigger_plugins        : string list
  ; source_plugins         : string list
  ; step_plugins           : string list
  ; authentication_plugins : (string * string) list
  }

let get_plugins () =
  let extract { content } =
    try
      let obj = from_string content in
      let res =
        { trigger_plugins =
            obj |> member "trigger_plugins" |> to_list |> List.map to_string
        ; source_plugins =
            obj |> member "source_plugins" |> to_list |> List.map to_string
        ; step_plugins =
            obj |> member "step_plugins" |> to_list |> List.map to_string
        ; authentication_plugins =
            obj |> member "authentication_plugins"
            |> to_list |> List.map (fun obj ->
                              ( obj |> member "name"  |> to_string
                              , obj |> member "title" |> to_string )
                            )
        }
      in Lwt.return res
    with | e -> print_endline "Error fetching plugins"; Lwt.fail e
  in Lwt.bind (perform_raw_url ~headers:(api_headers ()) "/api/plugins") extract
