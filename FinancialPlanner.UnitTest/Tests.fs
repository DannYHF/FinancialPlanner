module Tests

open System
open Xunit
open FinancialPlanner.Tokenizer

[<Fact>]
let ``My test`` () =
    let res = "someword 12341 : -  \"hello word!" |> tokenize
    Assert.True(true)