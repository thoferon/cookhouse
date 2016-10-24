open Sharp.Core
open Sharp.Core.Behaviour
open Sharp.Core.Network
open Sharp.Event
open Sharp.VDOM

open Utils
open Signin

let main_network () =
  let signin_container = get_element "#signin-container" in
  let signout_link     = get_element "#signout-link" in

  let open Network.Infix in
  click signout_link >>= fun signout_event ->
  unbound_event () >>= fun token_event ->

  let open Behaviour.Infix in
  let token = last ~init:None token_event in
  let signed_in = (function | Some _ -> true | None -> false) <$> token in

  let open Network.Infix in
  initially (fun () -> match Token.get () with
                       | Some token ->
                          let _ = trigger token_event (Some token) in ()
                       | None       -> ())
  >> react_ token Token.set
  >> react_ signout_event (fun _ ->
              let _ = Token.clear () in
              let _ = trigger token_event None in
              ()
            )

  >> vdom signin_container signed_in
          (function
           | true -> tag "span"
           | false ->
              E.div (signin_network token_event) |* ("class", "overlay")
          )

let () =
  (* Never stop this network *)
  let _ = start (main_network ()) in ()
