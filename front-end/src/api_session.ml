open Yojson.Basic
open Yojson.Basic.Util

open XmlHttpRequest

let signin plugin username password =
  let extract { code; content } =
    match code with
    | 200 ->
       begin
         try
           let obj = from_string content in
           let res = obj |> member "token" |> to_string in
           Lwt.return res
         with | e -> Lwt.fail e
       end
    | 401 -> Lwt.fail_with "Wrong credentials."
    | _ -> Lwt.fail_with "Unknown error."
  in
  let post_args =
    [ ("plugin",   `String (Js.string plugin))
    ; ("username", `String (Js.string username))
    ; ("password", `String (Js.string password))
    ]
  in Lwt.bind (perform_raw_url ~post_args "/api/signin") extract
