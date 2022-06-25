module FinancialPlanner.UICommands

open System
open FinancialPlanner.Error
open FinancialPlanner.CommandParameters
open FinancialPlanner.Form

type ShowSpendingsCommand =
    { FilterParameters: CommandParameter list }

type CreateExpectedSpendingCommand =
    { Form: CreateExpectedSpendingForm }    

type Command =
    | ShowSpendings of ShowSpendingsCommand
    | ClearConsole
    | CreateExpectedSpending of CreateExpectedSpendingCommand


let ShowSpendingsCommandName = "show"
let ClearConsoleCommandName = "clear"
let CreateExpectedSpendingName = "createExpected"

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
            | p -> Error (NotSuitableParameter (ShowSpendingsCommandName, p |> toParameterName))       
        | Error error -> Error error
    | [] -> command

let buildShowSpendingsCommand =
    buildShowSpendingsCommandRec (Ok { FilterParameters = [] })

let toCommandName command =
    match command with
    | ShowSpendings _ -> ShowSpendingsCommandName
    | ClearConsole -> ClearConsoleCommandName
    | CreateExpectedSpending _ -> CreateExpectedSpendingName

let resolveCommand (input: string) : Result<Command, CommandError list> =
    if input |> String.IsNullOrEmpty then
        Error [ ParsingFailed "Looks like as though input empty O_o" ]
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
            | _ -> Error [ UndefinedCommand $"Command name: %s{cmdName}" ]
        else
            Error errors
