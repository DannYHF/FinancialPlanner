module FinancialPlanner.UICommands

open System
open FinancialPlanner.Domain.Spending
open FinancialPlanner.Error
open FinancialPlanner.CommandParameters

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

let buildCreateExpectedSpendingCommand (parameters: CommandParameter list) =
    if parameters.Length = 2 then
        let estimatedCost = parameters |> List.tryFind (fun u -> match u with | EstimatedCostParameter _ -> true | _ -> false)
        let expenditureObject = parameters |> List.tryFind (fun u -> match u with | ExpenditureObjectParameter _ -> true | _ -> false)
        match (estimatedCost, expenditureObject) with
        | Some (EstimatedCostParameter ec), Some (ExpenditureObjectParameter eo) ->
               { Form = { EstimatedCost = ec.EstimatedCost
                          ExpenditureObject = eo.Object } } |> Ok
        | _ -> (MandatoryParametersAreNotFilled [ EstimatedCostParameterName; ExpenditureObjectParameterName ]) |> Error 
    elif parameters.Length < 2 then
        (ParsingFailed "Too few parameters are specified.") |> Error
    else
        (ParsingFailed "Too many parameters are specified.") |> Error

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
            input.Split " "
            |> Array.toList
            |> List.where (fun u -> not (u |> String.IsNullOrEmpty))

        let parsedParameters = words |> List.skip 1 |> parseParams
        let cmdName = (words |> List.take 1).Head
        let errors = parsedParameters |> Helper.getErrors
        let parameters = parsedParameters |> Helper.getResults

        if errors.IsEmpty then
            match cmdName with
            | show when show = ShowSpendingsCommandName ->
                match (parameters |> buildShowSpendingsCommand) with
                | Ok cmd -> Ok (ShowSpendings <| cmd)
                | Error error -> Error [ error ]
            
            | createEx when createEx = CreateExpectedSpendingName ->
                match (parameters |> buildCreateExpectedSpendingCommand) with
                | Ok cmd -> Ok (CreateExpectedSpending <| cmd)
                | Error error -> Error [ error ]
                
            | clear when clear = ClearConsoleCommandName -> Ok ClearConsole
            
            | _ -> Error [ UndefinedCommand $"Command name: %s{cmdName}" ]
        else
            Error errors
