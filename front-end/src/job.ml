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
open Utils

let status_class job = match status job with
  | InQueue    -> "job-in-queue"
  | InProgress -> "job-in-progress"
  | Success    -> "job-success"
  | Failure    -> "job-failure"
  | Rollbacked -> "job-rollbacked"
  | Aborted    -> "job-aborted"

let status_text job = match status job with
  | InQueue    -> "In queue"
  | InProgress -> "In progress"
  | Success    -> "Succeeded"
  | Failure    -> "Failed"
  | Rollbacked -> "Rollbacked"
  | Aborted    -> "Aborted"

let type_text job = match typ job with
  | Build     -> "Build"
  | PostBuild -> "Post build"

let job_minimal_view job =
  tag "div" |* ("class", "grid-small")
  |- (tag "div" |* ("class", "box " ^ status_class job)
      |- (tag "header"
          |- (tag "h3" |- text (project_id job))
          |- (tag "span" |- text ("#" ^ Api.Job.identifier job)))
      |- (tag "dl"
          |- (tag "dt" |- text "Type")
          |- (tag "dd" |- text (type_text job))
          |- (tag "dt" |- text "Status")
          |- (tag "dd" |- text (status_text job)))
      |- (tag "a" |* ("href", CVCVF.to_fragment Routes.job (project_id job)
                                                (Api.Job.identifier job))
          |- text "More"))

let phase_text result = match phase result with
  | Run      -> "run"
  | Rollback -> "rollback"

let job_result_output_network result container =
  let open Network.Infix in
  (on ~init:"" ~f:(fun s s' -> s ^ s') <$> event ()) >>= fun output ->
  every 2. () >>= fun tick ->

  let fetch_output str =
    plug_lwt output (get_job_result_output
                       (Api.JobResult.identifier result)
                       (String.length str))
  in

  initially (fun () -> fetch_output "")
  >> react tick output (fun _ str ->
             match end_time result with
             | None -> fetch_output str
             | Some _ -> ()
           )
  >> vdom container output (fun output -> tag "pre" |- text output)

let job_result_view show_phase result =
  let subtitle =
    "Start: " ^ human_readable_date (start_time result)
    ^ (match end_time result with
       | None -> "" | Some t -> ", end: " ^ human_readable_date t)
    ^ (match error result with
       | None -> "" | Some err -> ", error: " ^ err)
  in
  tag "section"
  |- (tag "header"
      |- (tag "h2" |- text (if show_phase
                            then "Output (phase: " ^ phase_text result ^ ")"
                            else "Output"))
      |- (tag "p" |* ("class", "subtitle")
          |- text subtitle))
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

let job_network menu_highlight projects project_id job_id container =
  let open Network.Infix in
  (on ~init:None ~f:(fun _ dat -> Some dat) <$> event ()) >>= fun job_data ->
  every 5. () >>= fun tick ->

  initially (fun () ->
      let _ = trigger menu_highlight (Project project_id) in
      plug_lwt job_data (get_job job_id)
    )

  >> react_ tick (fun () -> plug_lwt job_data (get_job job_id))

  >> vdom container job_data
          (function
           | None -> tag "span" |- text "Loading job..."
           | Some (job, deps, results) -> view job deps results
          )
