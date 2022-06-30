module FinancialPlanner.ConsoleUI

open System
open FinancialPlanner.Domain
open FinancialPlanner.Domain.Spending
open FinancialPlanner.Data
open FinancialPlanner.UICommandParameter
open FinancialPlanner.UICommands


let spendingPresentation spending =
    match spending with
    | Actual a ->
        $"  Id: %A{a.Id}\n"
        + $"  Creation date: {a.CreationDate:``dd-MM-yyyy``}\n"
        + $"  Expenditure object: %s{a.ExpenditureObject}\n"
        + $"  Estimated amount of money {a.EstimatedCost.Amount:N2} %c{a.EstimatedCost.Currency.PostFix}\n"
        + $"  Spent date: {a.SpentDate:``dd-MM-yyyy``}"
        + $"  Actual money spent: {a.ActualCost.Amount:N2} %c{a.ActualCost.Currency.PostFix}"
    | Expected e ->
        $"  Id: %A{e.Id}\n"
        + $"  Creation date: {e.CreationDate:``dd-MM-yyyy``}\n"
        + $"  Expenditure object: %s{e.ExpenditureObject}\n"
        + $"  Estimated amount of money {e.EstimatedCost.Amount:N2} %c{e.EstimatedCost.Currency.PostFix}"

let executeCommand command =
    async {
        let ctx = JsonDataContext ()
        let! spendings = ctx.getSpendings ()

        match command with
        | ShowSpendings cmd ->
            match spendings
                  |> Ok
                  |> (filterShowSpending cmd.FilterParameters) with
            | Ok s -> s |> List.iter (fun u -> printf $"%s{u |> spendingPresentation} \n \n")
            | Error e -> printfn $"%A{e}"
        | CreateExpectedSpending cmd ->
            do!
                cmd.Form
                |> createExpected
                |> Expected
                |> ctx.addSpending
        | MakeActualSpending cmd ->
            match (spendings |> List.tryFind (fun u -> cmd.ExpectedSpendingId = (u |> getId))) with
            | Some (Expected ex) -> ex
                                    |> makeActual (cmd.ActualCost, cmd.SpendDate)
                                    |> ignore
            | Some (Actual _) -> printfn $"Spending must be expected, but was actual"
            | None -> printfn $"Spending with guid %A{cmd.ExpectedSpendingId}"
        | ClearConsole -> Console.Clear ()

        do! ctx.saveChanges
    }
