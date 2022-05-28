open System
open FinancialPlanner.Data
open FinancialPlanner.Domain

let spending: Spending = Expected {
    Id = SpendingId(Guid.NewGuid())
    CreateDate = DateTime.Now
    ExpenditureObject = "Монитор"
    ExpenditureObjectDescription = "Хз какой"
    EstimatedAmountOfMoney = {
        Amount = 50000m
        Currency = Currency.Ruble
    }
}

let ctx = JsonDataContext "Data/Spendings.json"

    


module Program =
    [<EntryPoint>]
    let main args =
        ctx.addSpending(spending) |> Async.RunSynchronously
        ctx.saveChanges |> Async.RunSynchronously 
        0