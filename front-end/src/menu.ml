open Sharp.Core
open Sharp.VDOM
open Sharp.Router

open Behaviour
open Network

open Routes

type t =
  | Overview
  | Projects
  | Project of string

let view highlight projects =
  let project_ids = List.map Api.Project.identifier projects in
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
               ) project_ids))

let menu_network highlight projects container =
  vdom container highlight (fun highlight -> view highlight projects)
