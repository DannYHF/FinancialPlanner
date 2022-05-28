module FinancialPlanner.Data

open System.IO
open FSharp.Json
open FinancialPlanner.Domain

type JsonDataContext(dataFilePath: string) =
    let mutable spendings: Spending list = []
    let mutable dataLoaded = false
   
    member this.Spendings = spendings

    member this.saveChanges =
        async {
            let json = Json.serialize spendings
            File.Delete dataFilePath
            do!
                File.WriteAllTextAsync(dataFilePath, json)
                |> Async.AwaitTask
        }

    member this.addSpending(spending: Spending) = async {
        if not dataLoaded then
            do! this.loadSpendings()
            dataLoaded <- true

        spendings <- spending :: this.Spendings
    }

    
    member private this.loadSpendings() = async {
        let! json = this.readLines dataFilePath
        let uploadModels = Json.deserialize<Spending list> json
        spendings <- uploadModels
    }
        
        
    member private this.readLines(filePath: string) =
        use sr = new StreamReader(filePath)
        sr.ReadToEndAsync() |> Async.AwaitTask