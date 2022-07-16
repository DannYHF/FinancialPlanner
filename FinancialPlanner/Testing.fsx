#load "Tokenizer.fs"

open FinancialPlanner.Tokenizer

let res = "cmd \"string string string\"" |> tokenize
