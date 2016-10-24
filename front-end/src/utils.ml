module Token = struct
  (* TODO: Fallback on something else when localStorage is not available *)

  let get () =
    match Js.Optdef.to_option Dom_html.window##.localStorage with
    | Some storage -> Js.Opt.to_option
                        (Js.Opt.map (storage##getItem (Js.string "token"))
                                    Js.to_string)
    | None -> None

  let set token =
    Js.Optdef.iter Dom_html.window##.localStorage
                   (fun storage ->
                     storage##setItem (Js.string "token") (Js.string token))

  let clear () =
    Js.Optdef.iter Dom_html.window##.localStorage
                   (fun storage -> storage##removeItem (Js.string "token"))
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
