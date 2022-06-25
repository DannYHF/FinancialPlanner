module FinancialPlanner.CommandParameters

open System
open Error

type CountCommandParameter = { Count: int }
type CommandParameter =
    | CountParameter of CountCommandParameter

let CountParameterName = "count"

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
    | _ -> Error UndefinedParameter

let parseParam (param: string): Result<CommandParameter, CommandError> =
    if(param.IndexOf("-") = 0 || param.Length < 4) then
        let nameAndVal = param.Substring(1, param.Length - 1).Split ':'
        if nameAndVal.Length = 2 then
            buildParam nameAndVal[0] nameAndVal[1]
        else
            Error ParsingFailed
    else
        Error ParsingFailed

let rec filterShowSpending filters items  =
    match filters with
    | param::tail ->
        match param with
        | CountParameter p -> (items |> Helper.safeTake p.Count) |> filterShowSpending tail
    | [] -> items
     
   
let parseParams (parameters: string list) =
    parameters |> List.map parseParam            