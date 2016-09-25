module History = struct
  class type history =
    object
      method pushState : 'a -> Js.js_string Js.t -> Js.js_string Js.t
                         -> unit Js.meth
    end

  let history = (Js.Unsafe.js_expr "window.history" : history Js.t)

  let push_state title path =
    history##pushState () (Js.string title) (Js.string path)
end
