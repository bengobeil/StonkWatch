module Client.Tests

open Client.Types
open Fable.Mocha

let pnl = testList "PnL" [
  testCase "Position Open PnL calculates correctly" <| fun _ ->
    let position = SeedData.positionInfo
    
    let expected = PositionOpenPnL (OpenPnL (PnL (100m<percent>)))
    let actual = PositionOpenPnL.calculate position
    Expect.equal actual expected "Open PnL calculates correctly"
]

let client = testList "Client" [ pnl ]

let all =
    testList "All"
        [
#if FABLE_COMPILER // This preprocessor directive makes editor happy
            Tests.shared
#endif
            client
        ]

[<EntryPoint>]
let main _ = Mocha.runTests all