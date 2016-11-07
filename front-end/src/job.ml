open Sharp.Ajax
open Sharp.Core
open Sharp.Router
open Sharp.Vdom
open Sharp.Ticker

open Behaviour
open Network

open Api.Job
open Api.JobResult
open Menu

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
      |- (tag "span" |- text ("#" ^ Api.Job.identifier job)))
  |- (tag "dl"
      |- (tag "dt" |- text "Type")
      |- (tag "dd" |- text (type_text job))
      |- (tag "dt" |- text "Status")
      |- (tag "dd" |- text (status_text job)))
  |- (tag "a" |* ("href", CVCVF.to_fragment Routes.job (project_id job)
                                            (Api.Job.identifier job))
      |- text "More")

let phase_text result = match phase result with
  | Run      -> "run"
  | Rollback -> "rollback"

let job_result_output_network result container =
  let open Network.Infix in
  (on ~init:"" ~f:(fun s s' -> s ^ s') <$> event ()) >>= fun output ->
  every 10. () >>= fun tick ->

  initially (fun () ->
      plug_lwt output (get_job_result_output
                         (Api.JobResult.identifier result) 0)
    )
  >> react tick output (fun _ str ->
             plug_lwt output (get_job_result_output
                                (Api.JobResult.identifier result)
                                (String.length str))
           )
  >> vdom container output (fun output -> tag "pre" |- text output)

let job_result_view show_phase result =
  tag "section"
  |- (tag "h2" |- text (if show_phase
                        then "Output (phase: " ^ phase_text result ^ ")"
                        else "Output"))
  |- E.div (job_result_output_network result)

let view job deps results =
  tag "article"
  |- (tag "header"
      |- (tag "h1" |- text ("Job #" ^ Api.Job.identifier job)))
  |- (tag "dl"
      |- (tag "dt" |- text "Project")
      |- (tag "dd"
          |- (tag "a"
              |* ("href", CVF.to_fragment Routes.project (project_id job))
              |- text (project_id job)))
      |- (tag "dt" |- text "Type")
      |- (tag "dd" |- text (type_text job))
      |- (tag "dt" |- text "Status")
      |- (tag "dd" |- text (status_text job)))
  |- (match deps with
      | [] -> tag "span"
      | _  -> tag "section"
              |- (tag "h2" |- text "Dependencies")
              |- (tag "div" |* ("class", "grid")
                  |+ List.map job_minimal_view deps))
  |+ let show_phase = List.length results != 1 in
     List.map (job_result_view show_phase) results

let actual_network job deps results container =
  vdom_ container (fun () -> view job deps results)

let job_network menu_highlight projects project_id job_id container =
  let open Network.Infix in
  event () >>= fun job_data ->

  initially (fun () ->
      let _ = trigger menu_highlight (Project project_id) in
      plug_lwt job_data (get_job job_id)
    )

  >> vdom container job_data
          (function
           | None -> tag "span" |- text "Loading job..."
           | Some (job, deps, results) ->
              E.div (actual_network job deps results)
          )
