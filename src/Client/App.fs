module App

open System
open Fable.Remoting.Client
open Sutil
open Sutil.DOM
open Sutil.Bulma
open Sutil.Attr
open Sutil.Styling
open Feliz
open type Feliz.length
open Shared.Types

module Color =
  [<Literal>]
  let lightGrey = "#EEEEEE"
  
[<AutoOpen>]
module SutilOperators =
  let (|>>) x f =
    x
    |> Store.map f
    |> Store.distinct
    
  let (>>==) x f = Bind.fragment x f
  
module Bulma =
  let createElement el className =
    fun props -> el <| [ class' className ] @ props

  let navbar = createElement Html.nav "navbar"
  let level = createElement Html.nav "level"
  let table = createElement Html.table "table"

  module Navbar =
    let brand = createElement Html.div "navbar-brand"
    let item = createElement Html.a "navbar-item"

  module Level =
    let left = createElement Html.div "level-left"
    let right = createElement Html.div "level-right"
    let item = createElement Html.div "level-item"

type PortfolioTab =
  | Positions
  | Balances

type Model =
  { Portfolio: Portfolio
    CurrentPortfolioTab: PortfolioTab }

type Message =
  | SelectedPaneChanged of PortfolioTab
  | FetchPortfolio
  | PortfolioFetched of Portfolio
  
  
let portfolioApi =
  Remoting.createApi()
  |> Remoting.withRouteBuilder Route.builder
  |> Remoting.buildProxy<IPortfolioApi>
  
let init (): Model * Cmd<Message> =
  { Portfolio = { Positions = [] ; Balances = Undefined }
    CurrentPortfolioTab = Positions }, Cmd.ofMsg FetchPortfolio

let update (msg: Message) (model: Model): Model * Cmd<Message> =
  match msg with
  | SelectedPaneChanged portfolioTab ->
      let model =
        if portfolioTab <> model.CurrentPortfolioTab then
          { model with
              CurrentPortfolioTab = portfolioTab }
        else
          model
          
      model, Cmd.none
      
  | FetchPortfolio ->
    let msg = async {
      let! portfolio = portfolioApi.getPortfolio () 
      return PortfolioFetched portfolio
    }
    model, Cmd.OfAsync.result msg
      
  | PortfolioFetched portfolio ->
    { model with Portfolio = portfolio }, Cmd.none

let mainStyleSheet =
  Sutil.Bulma.withBulmaHelpers [ rule "nav.navbar" [ Css.borderBottom (px 1, borderStyle.solid, Color.lightGrey)]
                                 rule "div.body" [ Css.height (vh 100) ]
                                 rule ".full-height" [ Css.height (length.percent 100)]
                                 rule "span.pnl-percent"
                                   [ Css.fontSize (length.em 1.1)
                                     Css.fontWeight 500 ]

                                 rule ".pnl-percent.positive" [ Css.color "green" ]
                                 rule ".pnl-percent.negative" [ Css.color "red" ]
                                 rule
                                   "button.button.selected"
                                   [ Css.backgroundColor "#6A42B7"
                                     Css.color "white" ] ]

[<RequireQualifiedAccess>]
module PnL =
  let span (PnL percentage) =
    Html.span [ class' "pnl-percent"
                if percentage >= 0.m<percent> then
                  class' "positive"
                else
                  class' "negative"

                Html.text $"""{percentage}{"%"}""" ]
    
module Navbar =
  
  let section =
    Bulma.navbar [ Bulma.Navbar.brand [ Bulma.Navbar.item [ Html.h5 [ Html.text "STONK" ] ] ] ]

module SummaryPage =
  open Bulma

  let positionsTable (positionsStore: IObservable<PositionInfo list>) =
    let header =
      Html.thead [ Html.tr [ Html.th [ Html.text "Symbols" ]
                             Html.th [ Html.text "Open price" ]
                             Html.th [ Html.text "Current price" ]
                             Html.th [ Html.abbr [ attr ("title", "Open quantity") ]
                                       Html.text "Qty" ]
                             Html.th [ Html.abbr [ attr ("title", "Open profit and loss") ]
                                       Html.text "Open PnL" ] ] ]

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
        
      let symbolText = Stock.getSymbolString position.Stock
        
      Html.tr [ Html.td [ Html.text symbolText ]
                Html.td [ Html.text openPrice ]
                Html.td [ Html.text currentPrice ]
                Html.td [ Html.text openQtyString ]
                Html.td [ PnL.span pnl ] ]

    let rows positions =
      positions |> List.map getRowFromPositionInfo

    let getTableFromPositions positions = Bulma.table <| header :: rows positions

    positionsStore >>== getTableFromPositions

  let pnlElement (title: string) pnl =
    Level.item [ bulma.container [ style [ Css.textAlignCenter ]
                                   Html.h5 [ Html.text title; class' "mb-2" ]

                                   PnL.span pnl ] ]

  let button dispatch portfolioTab isSelectedStore =
    let tabString = string portfolioTab
    Level.item [ bulma.button.button [ Html.text tabString
                                       onClick (fun _ -> dispatch <| SelectedPaneChanged portfolioTab) []
                                       bindClass isSelectedStore "selected" ] ]

  let level dispatch (selectedPaneStore: IObservable<PortfolioTab>) =
    
    let isPositionsSelected = selectedPaneStore |>> ((=) Positions)
    let isBalancesSelected = selectedPaneStore |>> ((=) Balances)

    level [ Level.left [ button dispatch Positions isPositionsSelected
                         button dispatch Balances isBalancesSelected ]

            Level.right [ pnlElement "Open PnL" (PnL -3.23m<percent>)
                          pnlElement "Day PnL" (PnL 3.23m<percent>) ] ]

  let contentView model dispatch =

    let currentTabStore = model |>> (fun m -> m.CurrentPortfolioTab)
    let portfolioStore = model |>> (fun m -> m.Portfolio)

    let getViewForSelectedPane =
      function
      | Positions ->
          let positionListStore = portfolioStore |>> (fun p -> p.Positions)

          Html.div [ positionsTable positionListStore ]
          
      | Balances -> Html.text "Not done yet"

    bulma.section [ Html.div [ style [ Css.backgroundColor Color.lightGrey ]
                               bulma.container [ class' "p-5"

                                                 Html.h3 [ Html.text "Account summary" ]
                                                 bulma.container [ class' "pt-5"

                                                                   level dispatch currentTabStore
                                                                   currentTabStore >>== getViewForSelectedPane ] ] ] ]

module Main =
  let section model dispatch =
    Html.div [ class' "full-height"
      
               bulma.columns [ class' "full-height"
      
                               bulma.column [ column.is2
                                              style [ Css.backgroundColor Color.lightGrey ] ]
      
                               bulma.column [ SummaryPage.contentView model dispatch ] ] ]

let view () =
  let model, dispatch = Store.makeElmish init update ignore ()

  Html.div [ disposeOnUnmount [ model ]

             class' "body"
             Navbar.section
             Main.section model dispatch ]
  |> withStyle mainStyleSheet

// Start the app
view () |> mountElement "sutil-app"
