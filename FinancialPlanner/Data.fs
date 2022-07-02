namespace FinancialPlanner.Data

open System.Threading.Tasks
open System.IO
open FSharp.Json
open FinancialPlanner.Domain
open FinancialPlanner.Domain.Spending
open FinancialPlanner.Error
open FsToolkit.ErrorHandling
open FsToolkit.ErrorHandling.Operator.Result



type JsonDataContext() =
    let mutable spendings: Spending list = []
    let mutable dataLoaded = false
    let filePath = "../../../Data/Spendings.json"
   
    member this.saveChanges = async {
        let json = Json.serialize spendings
        do! this.writeLines json filePath |> Async.AwaitTask 
    }

    member this.add(spending: Spending) = async {
        if not dataLoaded then
            let! _ = this.getSpendings()
            dataLoaded <- true

        spendings <- spending :: spendings
    }
    
    member this.delete(spendingId: SpendingId): Async<unit option> = async {
        if not dataLoaded then
            let! _ = this.getSpendings()
            dataLoaded <- true

        let idxForRemove = spendings |> List.tryFindIndex (fun u -> u |> getId = spendingId)
        return
            match idxForRemove with
            | Some idx -> (spendings <- spendings |> List.removeAt idx) |> Some
            | None -> None
    }
    
    member this.put(spending: Spending) = async {
        if not dataLoaded then
            let! _ = this.getSpendings()
            dataLoaded <- true

        let! _ = this.delete (spending |> getId)
        
        do! this.add spending
    }
    
    member this.getSpendings(): Async<Spending list> = async {
        let! json = this.readLines filePath |> Async.AwaitTask
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
