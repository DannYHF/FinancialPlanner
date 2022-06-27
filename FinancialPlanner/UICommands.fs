module FinancialPlanner.UICommands

open System
open FinancialPlanner.Domain
open FinancialPlanner.Error
open FinancialPlanner.Utils
open FinancialPlanner.UICommandParameter

type ShowSpendingsCommand =
    { FilterParameters: CommandParameter list }

type CreateExpectedSpendingCommand =
    { Form: CreateExpectedSpendingForm }    

type Command =
    | ShowSpendings of ShowSpendingsCommand
    | ClearConsole
    | CreateExpectedSpending of CreateExpectedSpendingCommand


let ShowSpendingsCommandName = "list"
let ClearConsoleCommandName = "clear"
let CreateExpectedSpendingName = "createExpected"

let rec buildShowSpendingsCommandRec
    (command: Result<ShowSpendingsCommand, CommandError>)
    (parameters: CommandParameter list)
    : Result<ShowSpendingsCommand, CommandError> =
    match parameters with
    | param :: tail ->
        match command with
        | Ok cmd ->
            match param with
            | CountParameter p ->
                 tail |> buildShowSpendingsCommandRec ({ cmd with FilterParameters = (CountParameter <| p) :: cmd.FilterParameters } |> Ok)
            | EstimatedCostParameter _ -> (ShowSpendingsCommandName, param |> toParameterName) |> NotSuitableParameter |> Error
            | ExpenditureObjectParameter _ -> (ShowSpendingsCommandName, param |> toParameterName) |> NotSuitableParameter |> Error
        | Error error -> error |> Error
    | [] -> command

let buildShowSpendingsCommand =
    buildShowSpendingsCommandRec (Ok { FilterParameters = [] })

let buildCreateExpectedSpendingCommand (parameters: CommandParameter list) =
    if parameters.Length = 2 then
        let estimatedCost = parameters |> List.tryFind (fun u -> match u with | EstimatedCostParameter _ -> true | _ -> false)
        let expenditureObject = parameters |> List.tryFind (fun u -> match u with | ExpenditureObjectParameter _ -> true | _ -> false)
        match (estimatedCost, expenditureObject) with
        | Some (EstimatedCostParameter ec), Some (ExpenditureObjectParameter eo) ->
               { Form = { EstimatedCost = ec.EstimatedCost
                          ExpenditureObject = eo.Object } } |> Ok
        | _ -> [ EstimatedCostParameterName; ExpenditureObjectParameterName ] |> MandatoryParametersAreNotFilled |> Error 
    elif parameters.Length < 2 then
        (ParsingFailed "Too few parameters are specified.") |> Error
    else
        (ParsingFailed "Too many parameters are specified.") |> Error

let toCommandName command =
    match command with
    | ClearConsole -> ClearConsoleCommandName
    | ShowSpendings _ -> ShowSpendingsCommandName
    | CreateExpectedSpending _ -> CreateExpectedSpendingName

let resolveCommand (input: string) : Result<Command, CommandError list> =
    if input |> String.IsNullOrEmpty then
        [ ParsingFailed "Looks like as though input empty O_o" ] |> Error
    else
        let words =
            input.Split " "
            |> Array.toList
            |> List.where (fun u -> u |> String.IsNullOrEmpty |> not)

        let parsedParameters = words |> List.skip 1 |> parseParams
        let cmdName = (words |> List.take 1).Head
        let errors = parsedParameters |> Result.getErrors
        let parameters = parsedParameters |> Result.getResults

        if errors.IsEmpty then
            match cmdName with
            | show when show = ShowSpendingsCommandName ->
                match (parameters |> buildShowSpendingsCommand) with
                | Ok cmd -> ShowSpendings <| cmd |> Ok
                | Error error -> [ error ] |> Error
            | createEx when createEx = CreateExpectedSpendingName ->
                match (parameters |> buildCreateExpectedSpendingCommand) with
                | Ok cmd -> CreateExpectedSpending <| cmd |> Ok
                | Error error -> [ error ] |> Error
            | clear when clear = ClearConsoleCommandName -> ClearConsole |> Ok
            | _ -> Error [ UndefinedCommand $"Command name: %s{cmdName}" ]
        else
            errors |> Error
