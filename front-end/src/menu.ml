open Sharp.Core
open Sharp.VDOM
open Sharp.Router
open Sharp.Ajax

open Behaviour
open Network

open Routes

type t =
  | Overview
  | Projects
  | Project of string

let view highlight projects =
  tag "ul"
  |- (tag "li"
      |- (tag "a"
          |* ("href", Final.to_fragment overview)
          |* ("class", if highlight = Overview
                       then "highlight" else "")
          |- text "Overview"))
  |- (tag "li"
      |- (tag "a" |- text "Projects")
      |- (tag "ul"
          |+ List.map (fun project_id ->
                 tag "li"
                 |- (tag "a"
                     |* ( "href"
                        , CVF.to_fragment Routes.project project_id )
                     |* ("class", if highlight = Project project_id
                                  then "highlight" else "")
                     |- text project_id)
               ) projects))


let menu_network highlight container =
  let open Network.Infix in
  (last ~init:[] <$> unbound_event ()) >>= fun projects ->

  let open Behaviour.Infix in
  let project_ids = List.map Api.Project.identifier <$> projects in
  let dat = (fun h ps -> (h, ps)) <$> highlight <*> project_ids in

  let open Network.Infix in
  initially (fun () ->
      plug_lwt project_ids (Api.Project.get_projects ())
    )
  >> vdom container dat (fun (highlight, projects) -> view highlight projects)
