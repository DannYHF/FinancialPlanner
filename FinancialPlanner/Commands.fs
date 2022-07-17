namespace FinancialPlanner

open System
open FinancialPlanner
open FinancialPlanner.Abstractions
open FinancialPlanner.Domain
open FinancialPlanner.Error
open FinancialPlanner.Utils
open FinancialPlanner.CommandParameter
open FinancialPlanner.Tokenizer
open FsToolkit.ErrorHandling

type ShowSpendingsCommand =
    { FilterParameters: CommandParameter list }

type CreateExpectedSpendingCommand = { Form: CreateExpectedSpendingForm }

type MakeActualSpendingCommand =
    { ExpectedSpendingId: SpendingId
      ActualCost: Money
      SpendDate: DateTime }

type DeleteSpendingCommand = { SpendingId: SpendingId }

type Command =
    | ClearConsole
    | GetShortStatistics
    | ShowSpendings of ShowSpendingsCommand
    | MakeActualSpending of MakeActualSpendingCommand
    | CreateExpectedSpending of CreateExpectedSpendingCommand
    | DeleteSpending of DeleteSpendingCommand

module Commands =
    let showSpendingsCommandName = "list"
    let clearConsoleCommandName = "clear"
    let createExpectedSpendingCommandName = "createExpected"
    let makeActualSpendingCommandName = "makeActual"
    let getShortStatisticsCommandName = "shortStats"
    let deleteSpendingCommandName = "delete"
    
    let buildShowSpendingsCommand =
        let rec buildShowSpendingsCommandRec
            (command: Result<ShowSpendingsCommand, Error>)
            (parameters: CommandParameter list)
            : Result<ShowSpendingsCommand, Error> =
            match parameters with
            | param :: tail ->
                match command with
                | Ok cmd ->
                    match param with
                    | CountParameter p ->
                        tail
                        |> buildShowSpendingsCommandRec (
                            { cmd with
                                  FilterParameters = (CountParameter <| p) :: cmd.FilterParameters }
                            |> Ok
                        )
                    | SpendingIdParameter _
                    | ActualCostParameter _
                    | SpendDateParameter _
                    | EstimatedCostParameter _
                    | ExpenditureObjectParameter _ ->
                        (showSpendingsCommandName, param |> toParameterName)
                        |> NotSuitableParameter
                        |> Error
                | Error error -> error |> Error
            | [] -> command
    
        buildShowSpendingsCommandRec (Ok { FilterParameters = [] })
    
    let buildCreateExpectedSpendingCommand (parameters: CommandParameter list) =
        if parameters.Length = 2 then
            let estimatedCost =
                parameters
                |> List.tryFind
                   ^ fun u ->
                       match u with
                       | EstimatedCostParameter _ -> true
                       | _ -> false
    
            let expenditureObject =
                parameters
                |> List.tryFind
                   ^ fun u ->
                       match u with
                       | ExpenditureObjectParameter _ -> true
                       | _ -> false
    
            match (estimatedCost, expenditureObject) with
            | Some (EstimatedCostParameter ec), Some (ExpenditureObjectParameter eo) ->
                { Form =
                      { EstimatedCost = ec
                        ExpenditureObject = eo } }
                |> Ok
            | _ ->
                [ EstimatedCostParameterName
                  ExpenditureObjectParameterName ]
                |> MandatoryParametersAreNotFilled
                |> Error
        elif parameters.Length < 2 then
            (ParsingFailed "Too few parameters are specified.")
            |> Error
        else
            (ParsingFailed "Too many parameters are specified.")
            |> Error
    
    let buildMakeActualSpendingCommand (parameters: CommandParameter list) =
        if parameters.Length = 3 then
            let spendDate =
                parameters
                |> List.tryFind
                   ^ fun u ->
                       match u with
                       | SpendDateParameter _ -> true
                       | _ -> false
    
            let actualCost =
                parameters
                |> List.tryFind
                   ^ fun u ->
                       match u with
                       | ActualCostParameter _ -> true
                       | _ -> false
    
            let spendingId =
                parameters
                |> List.tryFind
                   ^ fun u ->
                       match u with
                       | SpendingIdParameter _ -> true
                       | _ -> false
    
            match (spendDate, actualCost, spendingId) with
            | Some (SpendDateParameter sd), Some (ActualCostParameter ac), Some (SpendingIdParameter si) ->
                { ExpectedSpendingId = si
                  ActualCost = ac
                  SpendDate = sd }
                |> Ok
            | _ ->
                [ EstimatedCostParameterName
                  ExpenditureObjectParameterName ]
                |> MandatoryParametersAreNotFilled
                |> Error
        elif parameters.Length < 3 then
            "Too few parameters are specified."
            |> ParsingFailed
            |> Error
        else
            "Too many parameters are specified."
            |> ParsingFailed
            |> Error
    
    let buildDeleteSpendingCommand (parameters: CommandParameter list) =
        if parameters.Length = 1 then
            let spendingId =
                parameters
                |> List.tryFind
                   ^ fun u ->
                       match u with
                       | SpendingIdParameter _ -> true
                       | _ -> false
    
            match spendingId with
            | Some (SpendingIdParameter id) -> { SpendingId = id } |> Ok
            | _ ->
                [ SpendingIdParameterName ]
                |> MandatoryParametersAreNotFilled
                |> Error
        else
            "Not the right number of parameters"
            |> ParsingFailed
            |> Error
    
    
    let toCommandName command =
        match command with
        | ClearConsole -> clearConsoleCommandName
        | GetShortStatistics -> getShortStatisticsCommandName
        | ShowSpendings _ -> showSpendingsCommandName
        | CreateExpectedSpending _ -> createExpectedSpendingCommandName
        | MakeActualSpending _ -> makeActualSpendingCommandName
        | DeleteSpending _ -> deleteSpendingCommandName
    
    let rec mapShowSpendings (result: Result<ShowSpendingsCommand, Error>) : Result<Command, Error> =
        match result with
        | Ok r -> ShowSpendings <| r |> Ok
        | Error e -> e |> Error
    let rec mapMakeActualSpending (result: Result<MakeActualSpendingCommand, Error>) : Result<Command, Error> =
        match result with
        | Ok r -> MakeActualSpending <| r |> Ok
        | Error e -> e |> Error
    let rec mapCreateExpectedSpending (result: Result<CreateExpectedSpendingCommand, Error>) : Result<Command, Error> =
        match result with
        | Ok r -> CreateExpectedSpending <| r |> Ok
        | Error e -> e |> Error
    let rec mapDeleteSpending (result: Result<DeleteSpendingCommand, Error>) : Result<Command, Error> =
        match result with
        | Ok r -> DeleteSpending <| r |> Ok
        | Error e -> e |> Error
    
    let validateUserInput input =
        if input |> String.IsNullOrEmpty then
            "Looks like as though input empty O_o" |> ParsingFailed |> Error
        else
            input |> Ok
        
    let resolveCommand (input: string) : Result<Command, Error> =
        result {
            let! validated = input |> validateUserInput
            let! tokens = validated |> tokenize
            let! parameters = tokens |> List.skip 1 |> parseParams
            let cmdToken = (tokens |> List.take 1).Head
            
            match cmdToken.Source with
            | show when show = showSpendingsCommandName ->
                return! parameters |> buildShowSpendingsCommand |> mapShowSpendings
                
            | createEx when createEx = createExpectedSpendingCommandName ->
                return! parameters |> buildCreateExpectedSpendingCommand |> mapCreateExpectedSpending
                
            | makeActual when makeActual = makeActualSpendingCommandName ->
                return! parameters |> buildMakeActualSpendingCommand |> mapMakeActualSpending
                
            | delete when delete = deleteSpendingCommandName ->
                return! parameters |> buildDeleteSpendingCommand |> mapDeleteSpending
                
            | getStats when getStats = getShortStatisticsCommandName -> return! GetShortStatistics |> Ok
            
            | clear when clear = clearConsoleCommandName -> return! ClearConsole |> Ok
            
            | _ -> return! $"Command name: %s{cmdToken.Source}" |> UndefinedCommand |>  Error 
        }