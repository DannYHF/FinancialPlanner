namespace FinancialPlanner.ConsoleUI

open System
open FinancialPlanner.Data
open Microsoft.FSharp.Core
open FinancialPlanner.Domain

type ShowSpendingsCommand = {
    Count: int Option 
}

type Command =
    | ShowSpendings of ShowSpendingsCommand
    | ClearConsole

type BoolCommandParameter = BoolCommandParameter of name:string * value:bool
type IntCommandParameter = IntCommandParameter of name:string * value:int

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
    
    let buildParam name value: CommandParameter option =
        match value with
        | Bool b -> Some (BoolParameter <| BoolCommandParameter (name, b))
        | Int i -> Some (IntParameter <| IntCommandParameter (name, i))
        | _ -> None
    
    let parseParam (param: string): CommandParameter Option =
        if(param.IndexOf("-") = 0 || param.Length < 4) then
            let nameAndVal = param.Substring(1, param.Length - 2).Split ':'
            if nameAndVal.Length = 2 then
                buildParam nameAndVal[0] nameAndVal[1]
            else
                None
        else
            None
            
    let parseParams (parameters: string list) =
        parameters |> List.map parseParam
        
    let rec buildShowSpendingsCommand parameter: Command option =
        None
        

    let resolveCommand (input: string) =
        if input |> String.IsNullOrEmpty then
            None
        else    
            let words = input.ToLower().Split " " |> Array.toList |> List.where (fun u -> not (u |> String.IsNullOrEmpty))
            match words with 
            | fst::_ when fst = ShowSpendingsCommandName -> words |> parseParams  |> buildShowSpendingsCommand
            | fst::_ when fst = ClearConsoleCommandName -> Some ClearConsole
            | _ -> None
    

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