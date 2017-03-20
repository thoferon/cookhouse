open Sharp.Core
open Sharp.Ajax
open Sharp.Event
open Sharp.Form
open Sharp.Vdom
open Sharp.Ticker

open Signal
open Network

open Api.Plugin
open Api.Session
open Utils

let session_network () =
  let open Network.Infix in
  event () >>= fun signout_event ->
  event () >>= fun sinfo_event ->

  let open Signal.Infix in
  let sinfo = last ~init:None sinfo_event in
  let signed_in = (function | Some _ -> true | None -> false) <$> sinfo in

  let open Network.Infix in
  initially (fun () -> match SessionInfo.get () with
                       | Some sinfo ->
                          let _ = trigger sinfo_event (Some sinfo) in ()
                       | None -> ())
  >> react_ sinfo SessionInfo.set
  >> react_ signout_event (fun _ ->
              let _ = signout () in
              let _ = SessionInfo.clear () in
              let _ = trigger sinfo_event None in
              ()
            )
  >> return (signed_in, signout_event)

let signin_network success_event signin_container =
  let open Network.Infix in
  success_receiver () >>= fun plugin_reception ->
  let auth_plugins = last ~init:[]
                          ((fun ps -> ps.authentication_plugins)
                           <$?> plugin_reception)
  in
  initially (fun () -> plug_lwt plugin_reception (get_plugins ()))

  >> (last ~init:"" <$> event ()) >>= fun plugin ->
  (last ~init:"" <$> event ()) >>= fun username ->
  (last ~init:"" <$> event ()) >>= fun password ->
  event () >>= fun submission ->

  event () >>= fun signin_result ->
  every 1. () >>= fun _ -> (* to update messages *)

  let open Signal.Infix in
  let signin_data = (fun pl u pa -> (pl, u, pa))
                    <$> plugin <*> username <*> password
  and message = (function
                 | Some (Error (Failure str)) -> Some str
                 | Some (Error _) -> Some "Unknown error."
                 | _ -> None
                ) <$> last_for 5. signin_result
  in
  let vdom_data = (fun ps msg -> (ps, msg)) <$> auth_plugins <*> message in
  let sinfo =
    (function | Some (Ok x) -> Some x | _ -> None) <$> signin_result
  in

  let open Network.Infix in
  react submission signin_data (fun _ (plugin, username, password) ->
          plug_lwt_result signin_result (signin plugin username password)
        )

  >> react_ sinfo (fun sinfo ->
              let _ = trigger success_event (Some sinfo) in ()
            )

  >> vdom signin_container vdom_data (fun (plugins, message) ->
            tag "div" |* ("class", "overlay-box")
            |- (tag "h1" |- text "Authentication")
            |- (E.form (Sub.submit submission)
                |- (tag "span" |* ("class", "error-message")
                    |- text (match message with | None -> "" | Some s -> s))
                |- (tag "label" |* ("for", "plugin")
                    |- text "Authentication with")
                |- (E.select (Sub.text_field plugin)
                    |* ("name", "plugin")
                    |+ List.map (fun (name, descr) ->
                           tag "option" |* ("value", name)
                           |- text descr
                         ) plugins)
                |- (tag "label" |* ("for", "username") |- text "Username")
                |- (E.input (Sub.text_field username)
                    |* ("name", "username") |* ("autofocus", ""))
                |- (tag "label" |* ("for", "password") |- text "Password")
                |- (E.input (Sub.text_field password)
                    |* ("name", "password") |* ("type", "password"))
                |- (tag "input" |* ("type", "submit") |* ("value", "Sign in")))
          )
