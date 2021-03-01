module App

open Sutil
open Sutil.DOM.Html
open Sutil.DOM
open Sutil.Attr

type Model = { Counter: int }

type Message =
  | Increment
  | Decrement

let init (): Model = { Counter = 0 }

let update (msg: Message) (model: Model): Model =
  match msg with
  | Increment -> { model with Counter = model.Counter + 1 }
  | Decrement -> { model with Counter = model.Counter - 1 }

let getCounter m = m.Counter

let view () =
  // Creating the store "cell" for the whole model, from the update/init functions
  let model, dispatch = Store.makeElmishSimple init update ignore ()

  let mkButton textContent msg =
    button [ class' "button"
             onClick (fun _ -> dispatch msg) []
             text textContent ]

  let counterStore = model |> Store.map getCounter

  let counterText =
    // Bind.fragment takes a store, and a function that takes a real value (non-store, int in this case)
    // and returns some html
    Bind.fragment counterStore <| fun counterValue -> text $"Counter: {counterValue}"
  // Notice, this is the only "Bind" function call, so this is the ONLY part that can change.

  let buttonsDiv =
    div [ mkButton "-" Decrement
          mkButton "+" Increment ]

  div [ disposeOnUnmount [ model ]
        style [ Css.fontFamily "Arial, Helvetica, sans-serif"
                Css.margin 20 ]

        counterText
        buttonsDiv ]

// Start the app
view () |> mountElement "sutil-app"
