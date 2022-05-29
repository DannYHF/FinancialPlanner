open System
open System.Text
open FinancialPlanner.Domain
open FinancialPlanner.ConsoleUI.Command

let spending: Spending = Expected {
    Id = SpendingId(Guid.NewGuid())
    CreationDate = DateTime.Now
    ExpenditureObject = "Монитор"
    EstimatedAmountOfMoney = {
        Amount = 50000m
        Currency = Currency.Ruble
    }
}

module Program =
    [<EntryPoint>]
    let main args =
        //let ctx = JsonDataContext Data.filePath
        //spending |> ctx.addSpending |> Async.RunSynchronously
        //ctx.saveChanges |> Async.RunSynchronously
        Console.OutputEncoding <- Encoding.Unicode
        while true do
            printf ">>> "
            let command = Console.ReadLine() |> resolveCommand
            match command with
            | Some cmd -> cmd |> executeCommand |> Async.RunSynchronously
            | None -> printfn "Command not defined"
            printfn ""
        0