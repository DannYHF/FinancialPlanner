open System
open System.Text
open FinancialPlanner.Data
open FinancialPlanner.Domain
open FinancialPlanner.ConsoleUI
open FinancialPlanner.Commands

let expectedSpending: Spending =
    Expected
        { Id = SpendingId(Guid.NewGuid())
          CreationDate = DateTime.Now.AddDays(-5)
          ExpenditureObject = "Монитор"
          EstimatedCost = 50000m
          Currency = Currency.Ruble }

let actualSpending: Spending =
    Actual
        { Id = SpendingId(Guid.NewGuid())
          CreationDate = DateTime.Now.AddDays(-5)
          ExpenditureObject = "Монитор"
          EstimatedCost = 1200m
          Currency = Currency.Dollar
          ActualSpent = 1000m
          SpentDate = DateTime.Now }        

module Program =
    [<EntryPoint>]
    let main _ =
        //let ctx = JsonDataContext()
        //expectedSpending |> ctx.add |> Async.RunSynchronously
        //actualSpending |> ctx.add |> Async.RunSynchronously
        //ctx.saveChanges |> Async.RunSynchronously
        Console.OutputEncoding <- Encoding.Unicode
        Console.InputEncoding <- Encoding.Unicode

        while true do
            printf ">>> "
            let command = Console.ReadLine() |> resolveCommand

            match command with
            | Ok cmd -> cmd |> executeCommand |> Async.RunSynchronously
            | Error e -> printfn $"%A{e}"

            printfn ""

        0