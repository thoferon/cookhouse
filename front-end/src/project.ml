open Sharp.Core
open Sharp.Vdom
open Sharp.Ajax
open Sharp.Ticker

open Behaviour
open Network

open Api.Project
open Menu

let view project build_request build_enabled =
  let btn0 =
    (E.button (Sub.click build_request (fun _ -> ()))
     |- text "Build")
  in let btn =
       if build_enabled then btn0 else btn0 |* ("disabled", "")
  in tag "section"
     |- (tag "header"
         |- (tag "h1" |- text ("Project " ^ identifier project))
         |- btn)
     |- (tag "dl"
         |- (tag "dt" |- text "Source")
         |- (tag "dd" |- text (source_location project)))

let actual_network menu_highlight project container =
  let open Network.Infix in
  event () >>= fun build_request ->
  event () >>= fun build_accepted ->

  let open Behaviour.Infix in
  let build_enabled =
    (fun x y -> x == y)
    <$> toggle ~init:true build_request
    <*> toggle ~init:true build_accepted
  in

  let open Network.Infix in
  initially (fun () ->
      let _ = trigger menu_highlight (Project (identifier project)) in ()
    )
  >> react_ build_request (fun () ->
              plug_lwt build_accepted (build_project (identifier project))
            )
  >> vdom container build_enabled (view project build_request)

let project_network menu_highlight projects project_id container =
  try
    let project =
      List.find (fun project -> identifier project = project_id) projects
    in actual_network menu_highlight project container
  with | Not_found -> Network.return ()
