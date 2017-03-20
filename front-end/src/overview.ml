open Sharp.Core
open Sharp.Ajax
open Sharp.Ticker
open Sharp.Vdom

open Signal
open Network

open Api.Job
open Job
open Menu

let view pending_jobs =
  tag "section"
  |- (tag "header"
      |- (tag "h1" |- text "Pending jobs"))
  |- match pending_jobs with
     | [] -> tag "p" |- text "Nothing's going on."
     | _ ->
        tag "div" |* ("class", "grid")
        |+ List.map job_minimal_view pending_jobs

let overview_network menu_highlight container =
  let open Network.Infix in
  (last ~init:[] <$> event ()) >>= fun pending_jobs ->
  every 5. () >>= fun ticker ->

  initially (fun () ->
      let _ = trigger menu_highlight Overview in
      plug_lwt pending_jobs (get_pending_jobs ())
    )
  >> react_ ticker (fun () ->
              plug_lwt pending_jobs (get_pending_jobs ())
            )
  >> vdom container pending_jobs view
