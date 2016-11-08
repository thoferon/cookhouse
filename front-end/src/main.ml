open Sharp.Ajax
open Sharp.Core
open Sharp.Event
open Sharp.Router
open Sharp.Vdom

open Behaviour
open Network

open Job
open Menu
open Overview
open Project
open Session
open Utils

let routes menu_highlight projects container =
  [ Final.parse Routes.overview
                (fun _ -> overview_network menu_highlight container)
  ; CVF.parse Routes.project
              (fun project_id _ ->
                project_network menu_highlight projects project_id container)
  ; CVCVF.parse Routes.job
                (fun project_id job_id _ ->
                  job_network menu_highlight projects project_id job_id
                              container)
  ]

let signed_in_network signout_event container =
  let open Network.Infix in
  (last ~init:Overview <$> event ()) >>= fun menu_highlight ->
  (last ~init:[]       <$> event ()) >>= fun projects ->

  initially (fun () ->
      plug_lwt projects (Api.Project.get_projects ())
    )

  >> vdom container projects
          (function
           | [] -> tag "span" |- text "Loading projects..."
           | projects ->
              tag "div"
              |- E.nav (menu_network menu_highlight signout_event projects)
              |- E.article (fun node ->
                     router_ (routes menu_highlight projects node)
                   )
          )

let main_network container =
  let open Network.Infix in
  session_network () >>= fun (signed_in, signout_event) ->
  vdom container signed_in (function
        | true  -> E.div (signed_in_network signout_event)
        | false -> E.div (signin_network signed_in) |* ("class", "overlay")
       )

let () =
  let container = get_element "#container" in
  (* Never stop this network *)
  let _ = start (main_network container) in ()
