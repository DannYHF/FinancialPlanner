module FinancialPlanner.UICommands

open System
open FinancialPlanner.Error
open FinancialPlanner.CommandParameters

type ShowSpendingsCommand =
    { FilterParameters: CommandParameter list }

type Command =
    | ShowSpendings of ShowSpendingsCommand
    | ClearConsole


let ShowSpendingsCommandName = "show"
let ClearConsoleCommandName = "clear"

let rec buildShowSpendingsCommandRec
    (command: Result<ShowSpendingsCommand, CommandError>)
    (parameters: CommandParameter list)
    : Result<ShowSpendingsCommand, CommandError> =
    match parameters with
    | head :: tail ->
        match command with
        | Ok cmd ->
            match head with
            | CountParameter p ->
                buildShowSpendingsCommandRec
                    (Ok
                        { cmd with
                              FilterParameters = (CountParameter <| p) :: cmd.FilterParameters })
                    tail
        | Error error -> Error error
    | [] -> command

let buildShowSpendingsCommand =
    buildShowSpendingsCommandRec (Ok { FilterParameters = [] })

let resolveCommand (input: string) : Result<Command, CommandError list> =
    if input |> String.IsNullOrEmpty then
        Error [ ParsingFailed ]
    else
        let words =
            input.ToLower().Split " "
            |> Array.toList
            |> List.where (fun u -> not (u |> String.IsNullOrEmpty))

        let parsedParameters = words |> List.skip 1 |> parseParams
        let cmdName = (words |> List.take 1).[0]
        let errors = parsedParameters |> Helper.getErrors
        let parameters = parsedParameters |> Helper.getResults

        if errors.IsEmpty then
            match cmdName with
            | s when s = ShowSpendingsCommandName ->
                match (parameters |> buildShowSpendingsCommand) with
                | Ok cmd -> Ok(ShowSpendings <| cmd)
                | Error error -> Error [ error ]
            | c when c = ClearConsoleCommandName -> Ok ClearConsole
            | _ -> Error [ UndefinedCommand ]
        else
            Error errors
