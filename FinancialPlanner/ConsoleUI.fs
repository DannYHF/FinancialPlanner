module FinancialPlanner.ConsoleUI 

open System
open Microsoft.FSharp.Core
open FinancialPlanner.Domain
open FinancialPlanner.UICommands
open FinancialPlanner.Data


let spendingPresentation spending =
    match spending with
    | Actual a ->
        $"  Id: %A{a.Id}\n"
        + $"  Creation date: {a.CreationDate:``dd-MM-yyyy``}\n"
        + $"  Expenditure object: %s{a.ExpenditureObject}\n"
        + $"  Estimated amount of money {a.EstimatedAmountOfMoney.Amount:N2} %c{a.EstimatedAmountOfMoney.Currency.PostFix}\n"
        + $"  Spent date: {a.SpentDate:``dd-MM-yyyy``}"
        + $"  Actual money spent: {a.ActualMoneySpent.Amount:N2} %c{a.ActualMoneySpent.Currency.PostFix}\n"
    | Expected e ->
        $"  Id: %A{e.Id}\n"
        + $"  Creation date: {e.CreationDate:``dd-MM-yyyy``}\n"
        + $"  Expenditure object: %s{e.ExpenditureObject}\n"
        + $"  Estimated amount of money {e.EstimatedAmountOfMoney.Amount:N2} %c{e.EstimatedAmountOfMoney.Currency.PostFix}\n"

let executeCommand command =
    async {
        let ctx = JsonDataContext filePath
        let! spendings = ctx.getSpendings ()

        match command with
        | ShowSpendings cmd ->
            spendings
            |> List.take (
                match cmd.Count with
                | Some c -> c
                | None -> spendings.Length)
            |> List.iter (fun u -> printf $"%s{u |> spendingPresentation}")
        | ClearConsole -> Console.Clear()
    }
