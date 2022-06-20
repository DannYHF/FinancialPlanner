namespace FinancialPlanner.CommandParameters

open System
open FinancialPlanner.Error

type CountCommandParameter = { Count: int }
type CommandParameter = CountParameter of CountCommandParameter

module CommandParameters = 
    let CountParameterName = "count"

    let (|CountParameter|_|) (name: string, value: string) =
        let mutable res = 0
        let parsed = Int32.TryParse(value, &res)
        
        if parsed && name = CountParameterName then
            Some res 
        else
            None
            
    let buildParam name value: Result<CommandParameter, CommandError> =
        match (name, value) with
        | CountParameter i -> Ok (CountParameter <| { Count = i })
        | _ -> Error UndefinedParameterValueType
    
    let parseParam (param: string): Result<CommandParameter, CommandError> =
        if(param.IndexOf("-") = 0 || param.Length < 4) then
            let nameAndVal = param.Substring(1, param.Length - 2).Split ':'
            if nameAndVal.Length = 2 then
                buildParam nameAndVal[0] nameAndVal[1]
            else
                Error ParsingFailed
        else
            Error ParsingFailed
            
    let parseParams (parameters: string list) =
        parameters |> List.map parseParam            