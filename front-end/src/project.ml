open Sharp.Core
open Sharp.Vdom
open Sharp.Ajax

open Behaviour
open Network

open Api.Project
open Menu

let view project_id build_request build_enabled =
  let btn0 =
    (E.button (Sub.click build_request (fun _ -> ()))
     |- text "Build")
  in let btn =
       if build_enabled then btn0 else btn0 |* ("disabled", "")
  in tag "section"
     |- (tag "header"
         |- (tag "h1" |- text ("Project " ^ project_id))
         |- btn)

let project_network menu_highlight container project_id =
  let open Network.Infix in
  unbound_event () >>= fun build_request ->
  unbound_event () >>= fun build_accepted ->

  let open Behaviour.Infix in
  let build_enabled =
    (fun x y -> x == y)
    <$> toggle ~init:true build_request
    <*> toggle ~init:true build_accepted
  in

  let open Network.Infix in
  initially (fun () ->
      let _ = trigger menu_highlight (Project project_id) in ()
    )
  >> react_ build_request (fun () ->
              plug_lwt build_accepted (build_project project_id)
            )
  >> vdom container build_enabled (view project_id build_request)
