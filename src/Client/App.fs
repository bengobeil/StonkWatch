module App

open System
open Sutil
open Sutil.DOM.Html
open Sutil.DOM
open Sutil.Bulma
open Sutil.Attr
open Sutil.Styling

[<Measure>] type percent

module Color =
  [<Literal>]
  let lightGrey = "#EEEEEE"

module Bulma =
  let createElement el className =
    fun props -> el <| [ class' className ] @ props

  let navbar = createElement nav "navbar"
  let level = createElement nav "level"

  module Navbar =
    let brand = createElement div "navbar-brand"
    let item  = createElement a "navbar-item"

  module Level =
    let left = createElement div "level-left"
    let right = createElement div "level-right"
    let item = createElement div "level-item"

type SummaryInfo =
  | Positions
  | Balances

module SummaryInfo =
  let name summaryInfo =
    match summaryInfo with
    | Positions -> "Positions"
    | Balances -> "Balances"

type Model =
  { OpenPnL: decimal<percent>
    DayPnL: decimal<percent>
    SelectedPane: SummaryInfo }

type Message =
  | SelectedPaneChanged of SummaryInfo

let init (): Model = { OpenPnL = 3.35m<percent>; DayPnL = -2m<percent>; SelectedPane = Positions }

let update (msg: Message) (model: Model): Model =
  match msg with
  | SelectedPaneChanged summaryInfo ->
    if summaryInfo <> model.SelectedPane then
      { model with SelectedPane = summaryInfo }
    else
      model

let mainStyleSheet =
  Sutil.Bulma.withBulmaHelpers
    [ rule "nav.navbar" [ Css.borderBottom $"1px {Color.lightGrey} solid" ]
      rule "div.body" [ Css.height "100vh" ]
      rule ".full-height" [ Css.height "100%" ]
      rule "span.pnl-percent" [ Css.fontSize "1.1em"
                                Css.fontWeight "500" ]

      rule ".pnl-percent.positive" [ Css.color "green" ]
      rule ".pnl-percent.negative" [ Css.color "red" ]
      rule "button.button.selected" [ Css.backgroundColor "#6A42B7"
                                      Css.color "white" ]
    ]

module Navbar =
  open Bulma
  let section = navbar [ Navbar.brand [ Navbar.item [ h5 <| [ text "STONK" ] ] ] ]

module Main =
  open Bulma

  let pnlElement title (percentage: decimal<percent>) =
    let percentageSpan =
      span [ class' "pnl-percent"
             if percentage >= 0.m<percent> then
               class' "positive"
              else
                class' "negative"

             text $"""{percentage}{"%"}""" ]

    Level.item
      [ bulma.container [ style [ Css.textAlign "center" ]
                          h5 [ text title
                               class' "mb-2" ]

                          percentageSpan ] ]

  let button dispatch summaryInfo isSelectedStore =
    Level.item [ bulma.button [ text <| SummaryInfo.name summaryInfo
                                onClick (fun e -> dispatch <| SelectedPaneChanged summaryInfo) []
                                bindClass isSelectedStore "selected" ] ]

  let level dispatch (selectedPaneStore: IObservable<SummaryInfo>) =
    let isPositionsSelected = selectedPaneStore |> Store.map (function Positions -> true | _ -> false)
    let isBalancesSelected = selectedPaneStore |> Store.map (function Balances -> true | _ -> false)

    Bulma.level
      [ Level.left [ button dispatch Positions isPositionsSelected
                     button dispatch Balances isBalancesSelected ]

        Level.right [ pnlElement "Open PnL" -3.23m<percent>
                      pnlElement "Day PnL" 5.23m<percent> ] ]

  let contentView model dispatch =

    let selectedPaneStore =
      model
      |> Store.map (fun m -> m.SelectedPane)
      |> Store.distinct

    bulma.section
      [ div [ style [ Css.backgroundColor Color.lightGrey ]

              bulma.container
                [ class' "p-5"

                  h3 <| [ text "Account summary" ]
                  bulma.container [ class' "pt-5"

                                    level dispatch selectedPaneStore ] ] ] ]

  let section model dispatch =
    div [ class' "full-height"
          bulma.columns [ class' "full-height"
                          bulma.column [ column.is 2
                                         style [ Css.backgroundColor Color.lightGrey ] ]

                          bulma.column [ contentView model dispatch ] ] ]

let view () =
  let model, dispatch = Store.makeElmishSimple init update ignore ()


  div [
        disposeOnUnmount [ model ]

        class' "body"
        Navbar.section
        Main.section model dispatch ]
  |> withStyle mainStyleSheet

// Start the app
view () |> mountElement "sutil-app"
