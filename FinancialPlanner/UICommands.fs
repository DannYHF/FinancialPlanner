namespace FinancialPlanner

open FinancialPlanner.CommandParameters
open FinancialPlanner.Error
open CommandParameters
open System

type ShowSpendingsCommand = {
    Count: int option 
}

type Command =
    | ShowSpendings of ShowSpendingsCommand
    | ClearConsole

module UICommands =
    let ShowSpendingsCommandName = "show"
    let ClearConsoleCommandName = "clear"
    
      
    let rec buildShowSpendingsCommandRec (command: ShowSpendingsCommand option) parameters: Command option =
        match parameters with
        | CountParameter head::tail when head.Name = "count" ->
            buildShowSpendingsCommandRec ( Some({ Count = Some head.Value })) tail
        | _ -> None
    
    let buildShowSpendingsCommand = buildShowSpendingsCommandRec (Some { Count = None })

    let resolveCommand (input: string): Result<Command, CommandError> =
        if input |> String.IsNullOrEmpty then
            Error ParsingFailed
        else    
            let words = input.ToLower().Split " "
                             |> Array.toList
                             |> List.where (fun u -> not (u |> String.IsNullOrEmpty))
            let parameters = words |> List.skip 1 |> parseParams
            
            Error ParsingFailed             