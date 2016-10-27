open Sharp.Core
open Sharp.Core.Behaviour
open Sharp.Core.Network
open Sharp.Event
open Sharp.VDOM
open Sharp.Router

open Session
open Utils

let list_projects_network () =
  let open Network.Infix in
  initially (fun () -> print_endline "Project list network started")
  >> finally (fun () -> print_endline "Project list network stopped")

let project_network project_id =
  initially (fun () -> print_endline ("Project network started for " ^ project_id))

let routes container =
  [ CF.parse Routes.list_projects
             (fun () -> start (list_projects_network ()))
  ; CVF.parse Routes.show_project
              (fun project_id () -> start (project_network project_id))
  ]

let main_network () =
  let container    = get_element "#container" in
  let signout_link = get_element "#signout-link" in

  let open Network.Infix in
  session_network signout_link >>= fun signed_in ->
  vdom container signed_in
       (function
        | true -> E.div (fun node -> router (routes node))
        | false -> E.div (signin_network signed_in) |* ("class", "overlay")
       )

let () =
  (* Never stop this network *)
  let _ = start (main_network ()) in ()
