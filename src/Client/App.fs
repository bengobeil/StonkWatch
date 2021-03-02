module App

open System
open Client
open Sutil
open Sutil.DOM.Html
open Sutil.DOM
open Sutil.Bulma
open Sutil.Attr
open Sutil.Styling
open Client.Types

module Color =
  [<Literal>]
  let lightGrey = "#EEEEEE"

module Bulma =
  let createElement el className =
    fun props -> el <| [ class' className ] @ props

  let navbar = createElement nav "navbar"
  let level = createElement nav "level"
  let table = createElement table "table"

  module Navbar =
    let brand = createElement div "navbar-brand"
    let item = createElement a "navbar-item"

  module Level =
    let left = createElement div "level-left"
    let right = createElement div "level-right"
    let item = createElement div "level-item"

type PortfolioTab =
  | Positions
  | Balances

type Model =
  { Portfolio: Portfolio
    CurrentPortfolioTab: PortfolioTab }

type Message = SelectedPaneChanged of PortfolioTab

let init (): Model =
  { Portfolio = SeedData.portfolio
    CurrentPortfolioTab = Positions }

let update (msg: Message) (model: Model): Model =
  match msg with
  | SelectedPaneChanged portfolioTab ->
      if portfolioTab <> model.CurrentPortfolioTab then
        { model with
            CurrentPortfolioTab = portfolioTab }
      else
        model

let mainStyleSheet =
  Sutil.Bulma.withBulmaHelpers [ rule "nav.navbar" [ Css.borderBottom $"1px {Color.lightGrey} solid" ]
                                 rule "div.body" [ Css.height "100vh" ]
                                 rule ".full-height" [ Css.height "100%" ]
                                 rule
                                   "span.pnl-percent"
                                   [ Css.fontSize "1.1em"
                                     Css.fontWeight "500" ]

                                 rule ".pnl-percent.positive" [ Css.color "green" ]
                                 rule ".pnl-percent.negative" [ Css.color "red" ]
                                 rule
                                   "button.button.selected"
                                   [ Css.backgroundColor "#6A42B7"
                                     Css.color "white" ] ]

[<RequireQualifiedAccess>]
module PnL =
  let span (PnL percentage) =
    span [ class' "pnl-percent"
           if percentage >= 0.m<percent> then
             class' "positive"
           else
             class' "negative"

           text $"""{percentage}{"%"}""" ]
    
module Navbar =
  
  let section =
    Bulma.navbar [ Bulma.Navbar.brand [ Bulma.Navbar.item [ h5 [ text "STONK" ] ] ] ]

module SummaryPage =
  open Bulma

  let positionsTable (positionsStore: IObservable<PositionInfo list>) =
    let header =
      thead [ tr [ th [ text "Symbols" ]
                   th [ text "Open price" ]
                   th [ text "Current price" ]
                   th [ el "abbr" [ attr ("title", "Open quantity") ]
                        text "Qty" ]
                   th [ el "abbr" [ attr ("title", "Open profit and loss") ]
                        text "Open PnL" ] ] ]

    let getRowFromPositionInfo (position: PositionInfo) =
      let openQtyString =
        position.OpenQty
        |> ShareQuantity.get 
        |> string
        
      let openPrice =
        position.AverageOpenPrice
        |> AverageOpenPrice.get
        |> string
        
      let currentPrice =
        position.Stock.CurrentPrice
        |> CurrentStockPrice.get
        |> string
        
      let (PositionOpenPnL (OpenPnL pnl)) =
        position
        |> PositionOpenPnL.calculate
        
      tr [ td [ text <| Stock.getSymbolString position.Stock ]
           td [ text openPrice ]
           td [ text currentPrice ]
           td [ text openQtyString ]
           td [ PnL.span pnl ] ]

    let rows positions =
      positions |> List.map getRowFromPositionInfo

    let getTableFromPositions positions = Bulma.table <| header :: rows positions

    Bind.fragment positionsStore getTableFromPositions

  let pnlElement title pnl =
    Level.item [ bulma.container [ style [ Css.textAlign "center" ]
                                   h5 [ text title; class' "mb-2" ]

                                   PnL.span pnl ] ]

  let button dispatch portfolioTab isSelectedStore =
    Level.item [ bulma.button [ text <| string portfolioTab
                                onClick (fun _ -> dispatch <| SelectedPaneChanged portfolioTab) []
                                bindClass isSelectedStore "selected" ] ]

  let level dispatch (selectedPaneStore: IObservable<PortfolioTab>) =
    let isPositionsSelected =
      selectedPaneStore
      |> Store.map (function
           | Positions -> true
           | _ -> false)

    let isBalancesSelected =
      selectedPaneStore
      |> Store.map (function
           | Balances -> true
           | _ -> false)

    level [ Level.left [ button dispatch Positions isPositionsSelected
                         button dispatch Balances isBalancesSelected ]

            Level.right [ pnlElement "Open PnL" (PnL -3.23m<percent>)
                          pnlElement "Day PnL" (PnL 3.23m<percent>) ] ]

  let contentView model dispatch =

    let selectedPaneStore =
      model
      |> Store.map (fun m -> m.CurrentPortfolioTab)
      |> Store.distinct

    let portfolioStore =
      model
      |> Store.map (fun m -> m.Portfolio)
      |> Store.distinct

    let getViewForSelectedPane portfolio =
      function
      | Positions ->
          let positionListStore =
            portfolio
            |> Store.map (fun p -> p.Positions)
            |> Store.distinct

          positionsTable positionListStore
      | Balances -> text "Not done yet"

    bulma.section [ div [ style [ Css.backgroundColor Color.lightGrey ]
                          bulma.container [ class' "p-5"

                                            h3 [ text "Account summary" ]
                                            bulma.container [ class' "pt-5"

                                                              level dispatch selectedPaneStore
                                                              Bind.fragment selectedPaneStore
                                                              <| getViewForSelectedPane portfolioStore ] ] ] ]

module Main =
  let section model dispatch =
    div [ class' "full-height"

          bulma.columns [ class' "full-height"

                          bulma.column [ column.is 2
                                         style [ Css.backgroundColor Color.lightGrey ] ]

                          bulma.column [ SummaryPage.contentView model dispatch ] ] ]

let view () =
  let model, dispatch = Store.makeElmishSimple init update ignore ()


  div [ disposeOnUnmount [ model ]

        class' "body"
        Navbar.section
        Main.section model dispatch ]
  |> withStyle mainStyleSheet

// Start the app
view () |> mountElement "sutil-app"
