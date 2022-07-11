namespace FinancialPlanner

open System
open FinancialPlanner
open FinancialPlanner.Domain
open FinancialPlanner.Error
open FinancialPlanner.Utils

type CommandParameter =
    | CountParameter of Count: int
    | ExpenditureObjectParameter of Object: string
    | EstimatedCostParameter of EstimatedCost: Money
    | ActualCostParameter of ActualCost: Money
    | SpendDateParameter of SpendDate: DateTime
    | SpendingIdParameter of SpendingId: SpendingId

module CommandParameter =
    let CountParameterName = "count"
    let EstimatedCostParameterName = "estimatedCost"
    let ExpenditureObjectParameterName = "expenditureObject"
    let ActualCostParameterName = "actualCost"
    let SpendDateParameterName = "spendDate"
    let SpendingIdParameterName = "spendingId"
    
    let toParameterName param =
        match param with
        | CountParameter _ -> CountParameterName
        | EstimatedCostParameter _ -> EstimatedCostParameterName
        | ExpenditureObjectParameter _ -> ExpenditureObjectParameterName
        | ActualCostParameter _ -> ActualCostParameterName
        | SpendDateParameter _ -> SpendDateParameterName
        | SpendingIdParameter _ -> SpendingIdParameterName
    
    let (|SpendDateCommandParameter|_|) (name: string, value: string) =
        let mutable res = DateTime.Today
        let parsed = DateTime.TryParse(value, &res)
        
        if parsed && name = SpendDateParameterName then
            Some res 
        else
            None
        
    let (|ActualCostCommandParameter|_|) (name: string, value: string) =
        let parsedMoney = Money.tryParse value
        
        match parsedMoney with
        | Some m when name = ActualCostParameterName ->
            Some m
        | Some _    
        | None -> None
     
    let (|CountCommandParameter|_|) (name: string, value: string) =
        let mutable res = 0
        let parsed = Int32.TryParse(value, &res)
        
        if parsed && name = CountParameterName then
            Some res 
        else
            None
            
    let (|SpendingIdCommandParameter|_|) (name: string, value: string) =
        let mutable res = Guid.Empty
        let parsed = Guid.TryParse(value, &res)
        
        if parsed && name = SpendingIdParameterName then
            res |> SpendingId |> Some 
        else
            None            
    
    let (|EstimatedCostCommandParameter|_|) (name: string, value: string) =
        let parsedMoney = Money.tryParse value
        
        match parsedMoney with
        | Some m when name = EstimatedCostParameterName ->
            Some m
        | Some _    
        | None -> None
    
    let (|ExpenditureObjectCommandParameter|_|) (name: string, value: string) =
        if name = ExpenditureObjectParameterName then
            Some value
        else None    
            
    let buildParam name value: Result<CommandParameter, CommandError> =
        match (name, value) with
        | CountCommandParameter i -> CountParameter <| i |> Ok
        | EstimatedCostCommandParameter i -> EstimatedCostParameter <| i |> Ok
        | ExpenditureObjectCommandParameter i -> ExpenditureObjectParameter <| i |> Ok
        | ActualCostCommandParameter i -> ActualCostParameter <| i |> Ok
        | SpendDateCommandParameter i -> SpendDateParameter <| i |> Ok
        | SpendingIdCommandParameter i -> SpendingIdParameter <| i |> Ok
        | _ -> Error (UndefinedParameter  (name, value))
    
    let parseParam (param: string): Result<CommandParameter, CommandError> =
        if(param.IndexOf("-") = 0 || param.Length < 4) then
            let nameAndVal = param.Substring(1, param.Length - 1).Split ':'
            if nameAndVal.Length = 2 then
                buildParam nameAndVal[0] nameAndVal[1]
            else
                Error (ParsingFailed "Invalid parameter syntax. Should look like: -[ParameterName]:[ParameterValue]")
        else
            Error (ParsingFailed "Invalid parameter syntax. Should look like: -[ParameterName]:[ParameterValue]")
    
    let rec filterShowSpending filters items  =
        match items with
        | Ok l ->
            match filters with
            | param::tail ->
                match param with
                | CountParameter p -> l |> List.safeTake p |> Ok |> filterShowSpending tail
                | SpendDateParameter _
                | ActualCostParameter _
                | SpendingIdParameter _
                | EstimatedCostParameter _
                | ExpenditureObjectParameter _ -> param |> toParameterName |> ExpectedFilterParameter |> Error
            | [] -> items
        | Error e -> Error e   
         
    let parseParams (parameters: string list) =
        parameters |> List.map parseParam