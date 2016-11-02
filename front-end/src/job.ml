open Sharp.Vdom
open Sharp.Router

open Api.Job

let status_class job = match status job with
  | InQueue    -> "job-in-queue"
  | InProgress -> "job-in-progress"
  | Success    -> "job-success"
  | Failure    -> "job-failure"
  | Rollbacked -> "job-rollbacked"

let status_text job = match status job with
  | InQueue    -> "In queue"
  | InProgress -> "In progress"
  | Success    -> "Succeeded"
  | Failure    -> "Failed"
  | Rollbacked -> "Rollbacked"

let type_text job = match typ job with
  | Build     -> "Build"
  | PostBuild -> "Post build"

let job_minimal_view job =
  tag "div" |* ("class", "grid-small-box " ^ status_class job)
  |- (tag "header"
      |- (tag "h2" |- text (project_id job))
      |- (tag "span" |- text ("#" ^ identifier job)))
  |- (tag "dl"
      |- (tag "dt" |- text "Type")
      |- (tag "dd" |- text (type_text job))
      |- (tag "dt" |- text "Status")
      |- (tag "dd" |- text (status_text job)))
  |- (tag "a" |* ("href", CVF.to_fragment Routes.job (identifier job))
      |- text "More")
