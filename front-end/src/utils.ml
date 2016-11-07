open Yojson.Basic
open Yojson.Basic.Util

module SessionInfo = struct
  (* TODO: Fallback on something else when localStorage is not available *)

  let get () =
    match Js.Optdef.to_option Dom_html.window##.localStorage with
    | Some storage ->
       Js.Opt.to_option
         (Js.Opt.bind (storage##getItem (Js.string "token"))
                      (fun token ->
                        Js.Opt.map (storage##getItem (Js.string "plugin"))
                                   (fun plugin ->
                                     (Js.to_string token, Js.to_string plugin)
                                   )
                      )
         )
    | None -> None

  let set (token, plugin) =
    Js.Optdef.iter Dom_html.window##.localStorage
                   (fun storage ->
                     storage##setItem (Js.string "token")  (Js.string token);
                     storage##setItem (Js.string "plugin") (Js.string plugin)
                   )

  let clear () =
    Js.Optdef.iter Dom_html.window##.localStorage
                   (fun storage ->
                     storage##removeItem (Js.string "token");
                     storage##removeItem (Js.string "plugin")
                   )

  let http_headers () = match get () with
    | None -> []
    | Some (token, plugin) ->
       [("X-API-Token", token); ("X-API-Authentication-Plugin", plugin)]
end

let push_state title path =
  Dom_html.window##.history##pushState () (Js.string title)
                                       (Js.some (Js.string path))

exception Element_not_found of string

let get_element_in el selector =
  Js.Opt.get (el##querySelector (Js.string selector))
             (fun () -> raise (Element_not_found selector))

let get_element = get_element_in Dom_html.document

let add_class    el name = el##.classList##add    (Js.string name)
let remove_class el name = el##.classList##remove (Js.string name)

let parse_entity f obj =
  let id   = string_of_int (obj |> member "id" |> to_int)
  and data = obj |> member "data" in
  f id data
