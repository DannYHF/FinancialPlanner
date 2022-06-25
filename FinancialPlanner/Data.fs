module FinancialPlanner.Data

open System.Threading.Tasks
open System.IO
open FSharp.Json
open FinancialPlanner.Domain

let filePath = "../../../Data/Spendings.json"

type JsonDataContext(dataFilePath: string) =
    let mutable spendings: Spending list = []
    let mutable dataLoaded = false
   
    member this.saveChanges = async {
        let json = Json.serialize spendings
        do! this.writeLines json dataFilePath |> Async.AwaitTask 
    }

    member this.addSpending(spending: Spending) = async {
        if not dataLoaded then
            let! _ = this.getSpendings()
            dataLoaded <- true

        spendings <- spending :: spendings
    }

    
    member this.getSpendings() = async {
        let! json = this.readLines dataFilePath |> Async.AwaitTask
        let uploadModels = Json.deserialize<Spending list> json
        spendings <- uploadModels
        return spendings
    }
        
        
    member private this.readLines(filePath: string): string Task = task {
        use file = File.OpenRead(filePath)
        use sr = new StreamReader(file)
        return! sr.ReadToEndAsync()
    }

    
    member private this.writeLines (content: string) (filePath: string): Task = task {
        use file = File.Open(filePath, FileMode.Create)
        use sw = new StreamWriter(file)
        return! sw.WriteLineAsync(content)
    }
