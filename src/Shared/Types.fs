module Shared.Types

[<Measure>]
type percent

[<Measure>]
type price

type Symbol = Symbol of string
type StockPrice = StockPrice of decimal<price>

type Quantity = Quantity of uint
type ShareQuantity = ShareQuantity of Quantity

[<RequireQualifiedAccess>]
module ShareQuantity =
  let get (ShareQuantity (Quantity num)) = num

type PnL = PnL of decimal<percent>
type OpenPnL = OpenPnL of PnL
type DayPnL = DayPnL of PnL
type PositionOpenPnL = PositionOpenPnL of OpenPnL

type PositionDayPnL = PositionDayPnL of DayPnL
type PortfolioOpenPnl = PortfolioOpenPnl of OpenPnL

type PortfolioDayPnl = PortfolioDayPnl of DayPnL

type CurrentStockPrice = CurrentStockPrice of StockPrice

[<RequireQualifiedAccess>]
module CurrentStockPrice =
  let get (CurrentStockPrice (StockPrice price)) = price

type LastClosePrice = LastClosePrice of StockPrice

type AveragePrice = AveragePrice of decimal<price>
type AverageOpenPrice = AverageOpenPrice of AveragePrice

[<RequireQualifiedAccess>]
module AverageOpenPrice =
  let get (AverageOpenPrice (AveragePrice realPrice)) = realPrice

type Stock =
  { Symbol: Symbol
    CurrentPrice: CurrentStockPrice
    LastClosePrice: LastClosePrice }

[<RequireQualifiedAccess>]
module Stock =
  let getSymbolString ({ Symbol = (Symbol str) }) = str

type PositionInfo =
  { Stock: Stock
    OpenQty: ShareQuantity
    AverageOpenPrice: AverageOpenPrice }

module PositionOpenPnL =
  let toPositionOpenPnL decimalPercent =
    decimalPercent
    |> PnL
    |> OpenPnL
    |> PositionOpenPnL

  let calculate (positionInfo: PositionInfo): PositionOpenPnL =
    // ((current - open) / open) * 100
    let current =
      CurrentStockPrice.get positionInfo.Stock.CurrentPrice

    let open' = AverageOpenPrice.get positionInfo.AverageOpenPrice

    let pnlRatio = (current - open') / open'
    let pnl = pnlRatio * 100m<percent>

    toPositionOpenPnL pnl
    
  let get (PositionOpenPnL (OpenPnL (PnL percent))) = percent

type Balances = Undefined

type Portfolio =
  { Positions: PositionInfo list
    Balances: Balances }
