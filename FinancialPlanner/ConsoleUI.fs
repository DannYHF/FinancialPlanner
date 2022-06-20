namespace FinancialPlanner.ConsoleUI

open System
open FinancialPlanner.Data
open FinancialPlanner.Error
open Microsoft.FSharp.Core
open FinancialPlanner.Domain

type ShowSpendingsCommand = {
    Count: int option 
}

type Command =
    | ShowSpendings of ShowSpendingsCommand
    | ClearConsole

type BoolCommandParameter = {
    Name:string
    Value:bool
}
type IntCommandParameter = {
    Name:string
    Value:int
}

type CommandParameter =
    | BoolParameter of BoolCommandParameter
    | IntParameter of IntCommandParameter

module Command =
    let ShowSpendingsCommandName = "show"
    let ClearConsoleCommandName = "clear"
    
    let (|Bool|_|) (value: string) =
        let mutable res = false
        if Boolean.TryParse(value, &res) then Some res
        else None
        
    let (|Int|_|) (value: string) =
        let mutable res = 0
        if Int32.TryParse(value, &res) then Some res
        else None        
    
    let buildParam name value: Result<CommandParameter, CommandError> =
        match value with
        | Bool b -> Ok (BoolParameter <| { Name = name
                                           Value = b })
        | Int i -> Ok (IntParameter <| { Name = name
                                         Value = i })
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
        
    let rec buildShowSpendingsCommandRec (command: ShowSpendingsCommand option) parameters: Command option =
        match parameters with
        | IntParameter head::tail when head.Name = "count" ->
            buildShowSpendingsCommandRec ( Some({ Count = Some head.Value })) tail
        | BoolParameter head::tail -> None
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
            
    
    

    let spendingPresentation spending =
        match spending with
        | Actual a ->
            $"  Id: %A{a.Id}\n" +
            $"  Creation date: {a.CreationDate: ``dd-MM-yyyy``}\n" +
            $"  Expenditure object: %s{a.ExpenditureObject}\n" +
            $"  Estimated amount of money {a.EstimatedAmountOfMoney.Amount:N2} %c{a.EstimatedAmountOfMoney.Currency.PostFix}\n" +
            $"  Spent date: {a.SpentDate: ``dd-MM-yyyy``}" +
            $"  Actual money spent: {a.ActualMoneySpent.Amount:N2} %c{a.ActualMoneySpent.Currency.PostFix}\n"
        | Expected e ->
            $"  Id: %A{e.Id}\n" +
            $"  Creation date: {e.CreationDate: ``dd-MM-yyyy``}\n" +
            $"  Expenditure object: %s{e.ExpenditureObject}\n" +
            $"  Estimated amount of money {e.EstimatedAmountOfMoney.Amount:N2} %c{e.EstimatedAmountOfMoney.Currency.PostFix}\n"
            
    let executeCommand command = async {
        let ctx = JsonDataContext Data.filePath
        let! spendings = ctx.getSpendings()
        match command with
        | ShowSpendings _ -> spendings |> List.iter (fun u -> printf $"%s{u |> spendingPresentation}")
        | ClearConsole -> Console.Clear()
    }         