open Sharp.Core
open Sharp.Event
open Sharp.VDOM
open Sharp.Router
open Sharp.Ajax

open Behaviour
open Network

open Menu
open Session
open Project
open Utils

let overview_network menu_highlight container =
  let open Network.Infix in
  initially (fun () -> let _ = trigger menu_highlight Overview in ())
  >> initially (fun () -> print_endline "Project list network started")
  >> finally (fun () -> print_endline "Project list network stopped")

let routes menu_highlight projects container =
  [ Final.parse Routes.overview
                (fun _ -> overview_network menu_highlight container)
  ; CVF.parse Routes.project
              (fun project_id _ ->
                project_network menu_highlight projects project_id container)
  ]

let signed_in_network container =
  let open Network.Infix in
  (last ~init:Overview <$> event ()) >>= fun menu_highlight ->
  (last ~init:[] <$> event ())       >>= fun projects ->

  initially (fun () ->
      plug_lwt projects (Api.Project.get_projects ())
    )

  >> vdom container projects
          (function
           | [] -> tag "span" |- text "Loading projects..."
           | projects ->
              tag "div"
              |- E.nav (menu_network menu_highlight projects)
              |- E.div (fun node ->
                     router_ (routes menu_highlight projects node)
                   )
          )

let main_network container signout_link =
  let open Network.Infix in
  session_network signout_link >>= fun signed_in ->
  vdom container signed_in (function
        | true  -> E.div signed_in_network
        | false -> E.div (signin_network signed_in) |* ("class", "overlay")
       )

let () =
  let container    = get_element "#container" in
  let signout_link = get_element "#signout-link" in
  (* Never stop this network *)
  let _ = start (main_network container signout_link) in ()
