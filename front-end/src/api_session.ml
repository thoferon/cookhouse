open Yojson.Basic
open Yojson.Basic.Util

open XmlHttpRequest

open Utils

let signin plugin username password =
  let extract { code; content } =
    match code with
    | 200 ->
       begin
         try
           let token = from_string content |> to_string in
           Lwt.return (token, plugin)
         with | e -> prerr_endline "Can't parse token"; Lwt.fail e
       end
    | 401 -> Lwt.fail_with "Wrong credentials."
    | _ -> Lwt.fail_with "Unknown error."
  in
  let post_args =
    [ ("plugin",   `String (Js.string plugin))
    ; ("username", `String (Js.string username))
    ; ("password", `String (Js.string password))
    ]
  in Lwt.bind (perform_raw_url ~headers:(api_headers ())
                               ~post_args "/api/signin") extract

let signout () =
  match SessionInfo.get () with
  | None -> Lwt.return ()
  | Some (token, plugin) ->
     let post_args =
       [ ("plugin", `String (Js.string plugin))
       ; ("token",  `String (Js.string token))
       ]
     in Lwt.bind (perform_raw_url ~headers:(api_headers ())
                                  ~post_args "/api/signout")
                 (fun _ -> Lwt.return ())
