module FinancialPlanner.UICommands

open System
open FinancialPlanner.Abstractions
open FinancialPlanner.Domain
open FinancialPlanner.Error
open FinancialPlanner.Utils
open FinancialPlanner.UICommandParameter

type ShowSpendingsCommand =
    { FilterParameters: CommandParameter list }

type CreateExpectedSpendingCommand =
    { Form: CreateExpectedSpendingForm }
    
type MakeActualSpendingCommand =
    { ExpectedSpendingId: SpendingId
      ActualCost: Money
      SpendDate: DateTime }

type Command =
    | ClearConsole
    | GetShortStatistics
    | ShowSpendings of ShowSpendingsCommand
    | MakeActualSpending of MakeActualSpendingCommand
    | CreateExpectedSpending of CreateExpectedSpendingCommand

let ShowSpendingsCommandName = "list"
let ClearConsoleCommandName = "clear"
let CreateExpectedSpendingCommandName = "createExpected"
let MakeActualSpendingCommandName = "makeActual"
let getShortStatisticsCommandName = "shortStats"

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
            | SpendingIdParameter _
            | ActualCostParameter _
            | SpendDateParameter _
            | EstimatedCostParameter _
            | ExpenditureObjectParameter _ -> (ShowSpendingsCommandName, param |> toParameterName) |> NotSuitableParameter |> Error
        | Error error -> error |> Error
    | [] -> command

let buildShowSpendingsCommand =
    buildShowSpendingsCommandRec (Ok { FilterParameters = [] })

let buildCreateExpectedSpendingCommand (parameters: CommandParameter list) =
    if parameters.Length = 2 then
        let estimatedCost = parameters |> List.tryFind ^fun u -> match u with | EstimatedCostParameter _ -> true | _ -> false
        let expenditureObject = parameters |> List.tryFind ^fun u -> match u with | ExpenditureObjectParameter _ -> true | _ -> false
        match (estimatedCost, expenditureObject) with
        | Some (EstimatedCostParameter ec), Some (ExpenditureObjectParameter eo) ->
               { Form = { EstimatedCost = ec
                          ExpenditureObject = eo } } |> Ok
        | _ -> [ EstimatedCostParameterName; ExpenditureObjectParameterName ] |> MandatoryParametersAreNotFilled |> Error 
    elif parameters.Length < 2 then
        (ParsingFailed "Too few parameters are specified.") |> Error
    else
        (ParsingFailed "Too many parameters are specified.") |> Error
        
let buildMakeActualSpendingCommand (parameters: CommandParameter list) =
    if parameters.Length = 3 then
        let spendDate = parameters |> List.tryFind ^fun u -> match u with | SpendDateParameter _ -> true | _ -> false
        let actualCost = parameters |> List.tryFind ^fun u -> match u with | ActualCostParameter _ -> true | _ -> false
        let spendingId = parameters |> List.tryFind ^fun u -> match u with | SpendingIdParameter _ -> true | _ -> false
        match (spendDate, actualCost, spendingId) with
        | Some (SpendDateParameter sd), Some (ActualCostParameter ac), Some(SpendingIdParameter si) ->
               { ExpectedSpendingId = si
                 ActualCost = ac
                 SpendDate = sd } |> Ok
        | _ -> [ EstimatedCostParameterName; ExpenditureObjectParameterName ] |> MandatoryParametersAreNotFilled |> Error 
    elif parameters.Length < 3 then
        (ParsingFailed "Too few parameters are specified.") |> Error
    else
        (ParsingFailed "Too many parameters are specified.") |> Error        

let toCommandName command =
    match command with
    | ClearConsole -> ClearConsoleCommandName
    | GetShortStatistics -> getShortStatisticsCommandName
    | ShowSpendings _ -> ShowSpendingsCommandName
    | CreateExpectedSpending _ -> CreateExpectedSpendingCommandName
    | MakeActualSpending _ -> MakeActualSpendingCommandName

let resolveCommand (input: string) : Result<Command, CommandError list> =
    if input |> String.IsNullOrEmpty then
        [ ParsingFailed "Looks like as though input empty O_o" ] |> Error
    else
        let words =
            input.Split " "
            |> Array.toList
            |> List.where ^fun u -> u |> String.IsNullOrEmpty |> not

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
            | createEx when createEx = CreateExpectedSpendingCommandName ->
                match (parameters |> buildCreateExpectedSpendingCommand) with
                | Ok cmd -> CreateExpectedSpending <| cmd |> Ok
                | Error error -> [ error ] |> Error
            | makeActual when makeActual = MakeActualSpendingCommandName ->
                match (parameters |> buildMakeActualSpendingCommand) with
                | Ok cmd -> MakeActualSpending <| cmd |> Ok
                | Error error -> [ error ] |> Error
            | getStats when getStats = getShortStatisticsCommandName ->
                GetShortStatistics |> Ok
            | clear when clear = ClearConsoleCommandName -> ClearConsole |> Ok
            | _ -> Error [ UndefinedCommand $"Command name: %s{cmdName}" ]
        else
            errors |> Error
