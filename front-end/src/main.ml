open Sharp.Core
open Sharp.Core.Behaviour
open Sharp.Core.Network
open Sharp.Event
open Sharp.VDOM
open Sharp.Router

open Menu
open Session
open Project
open Utils

let overview_network menu_highlight container =
  let open Network.Infix in
  initially (fun () -> let _ = trigger menu_highlight Overview in ())
  >> initially (fun () -> print_endline "Project list network started")
  >> finally (fun () -> print_endline "Project list network stopped")

let routes menu_highlight container =
  [ Final.parse Routes.overview
                (overview_network menu_highlight container)
  ; CVF.parse Routes.project
              (fun project_id ->
                project_network menu_highlight container project_id)
  ]

let main_network () =
  let container    = get_element "#container" in
  let signout_link = get_element "#signout-link" in

  let open Network.Infix in
  session_network signout_link               >>= fun signed_in ->
  (last ~init:Overview <$> unbound_event ()) >>= fun menu_highlight ->

  vdom container signed_in
       (function
        | true ->
           tag "div"
           |- E.nav (menu_network menu_highlight)
           |- E.div (fun node -> router (routes menu_highlight node))
        | false -> E.div (signin_network signed_in) |* ("class", "overlay")
       )

let () =
  (* Never stop this network *)
  let _ = start (main_network ()) in ()
