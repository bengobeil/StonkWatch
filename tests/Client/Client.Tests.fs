module Client.Tests

open Fable.Mocha

let client = testList "Client" [
    testCase "Added todo" <| fun _ ->
        ()
]

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