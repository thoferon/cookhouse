open Sharp.Core
open Sharp.Vdom
open Sharp.Ajax
open Sharp.Ticker
open Sharp.Router

open Behaviour
open Network

open Api.Project
open Job
open Menu
open Routes

let view project build_request build_enabled jobs =
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
         |- (tag "dd"
             |- (tag "a"
                 |* ("href", source_location project)
                 |- text (source_location project))
             |- text (" (" ^ source_plugin project ^ ")"))
         |- (tag "dt" |- text "Dependencies")
         |- (tag "dd"
             |- match dependencies project with
                | []   -> text "None"
                | deps ->
                   tag "span" |* ("class", "enum")
                   |+ List.map (fun dep ->
                          tag "a"
                          |* ("class", "enum-element")
                          |* ("href", CVF.to_fragment Routes.project dep)
                          |- text dep
                        ) deps))
     |- (tag "div" |* ("class", "grid")
        |+ List.map job_minimal_view jobs)

let actual_network menu_highlight project container =
  let open Network.Infix in
  event () >>= fun build_request ->
  event () >>= fun build_accepted ->

  (last ~init:[] <$> event ()) >>= fun jobs ->
  every 15. () >>= fun ticker ->

  let open Behaviour.Infix in
  let build_enabled =
    (fun x y -> x == y)
    <$> toggle ~init:true build_request
    <*> toggle ~init:true build_accepted
  in
  let dat = (fun x y -> (x, y)) <$> build_enabled <*> jobs in

  let open Network.Infix in
  initially (fun () ->
      let _ = trigger menu_highlight (Project (identifier project)) in ()
    )

  >> react_ build_request (fun () ->
              plug_lwt build_accepted (build_project (identifier project))

            )

  >> initially (fun () ->
      plug_lwt jobs (Api.Job.get_project_jobs (identifier project))
    )
  >> react_ ticker (fun () ->
              plug_lwt jobs (Api.Job.get_project_jobs (identifier project))
            )

  >> vdom container dat (fun (build_enabled, jobs) ->
            view project build_request build_enabled jobs
          )

let project_network menu_highlight projects project_id container =
  try
    let project =
      List.find (fun project -> identifier project = project_id) projects
    in actual_network menu_highlight project container
  with | Not_found ->
          vdom container (Behaviour.pure ()) (fun _ -> text "Project not found")
