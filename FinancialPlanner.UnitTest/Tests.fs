module Tests

open System
open Xunit
open FinancialPlanner.Tokenizer

[<Fact>]
let ``My test`` () =
    let tokenizeRes = "createExpected -estimatedCost:40000r -expenditureObject:\"Кресло ортопедическое\"" |> tokenize
    Assert.True(true)