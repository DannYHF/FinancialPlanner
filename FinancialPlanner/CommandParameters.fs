module FinancialPlanner.CommandParameters

open System
open FinancialPlanner.Domain
open Error

type CountCommandParameter = { Count: int }
type ExpenditureObjectCommandParameter = { Object: string }
type EstimatedCostCommandParameter = { EstimatedCost: Money }
type CommandParameter =
    | CountParameter of CountCommandParameter
    | ExpenditureObjectParameter of ExpenditureObjectCommandParameter
    | EstimatedCost of EstimatedCostCommandParameter

let CountParameterName = "count"
let EstimatedCostParameterName = "estimatedCost"
let ExpenditureObjectParameterName = "expenditureObject"

let (|CountCommandParameter|_|) (name: string, value: string) =
    let mutable res = 0
    let parsed = Int32.TryParse(value, &res)
    
    if parsed && name = CountParameterName then
        Some res 
    else
        None
        
let buildParam name value: Result<CommandParameter, CommandError> =
    match (name, value) with
    | CountCommandParameter i -> Ok (CountParameter <| { Count = i })
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
    match filters with
    | param::tail ->
        match param with
        | CountParameter p -> (items |> Helper.safeTake p.Count) |> filterShowSpending tail
    | [] -> items
     
   
let parseParams (parameters: string list) =
    parameters |> List.map parseParam
    
let toParameterName param =
    match param with
    | CountParameter _ -> CountParameterName
    | EstimatedCost _ -> EstimatedCostParameterName
    | ExpenditureObjectParameter _ -> ExpenditureObjectParameterName