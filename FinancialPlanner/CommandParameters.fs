namespace FinancialPlanner

open System
open FinancialPlanner
open FinancialPlanner.Domain
open FinancialPlanner.Error
open FinancialPlanner.Utils
open Microsoft.FSharp.Core

type CommandParameter =
    | CountParameter of Count: int
    | ExpenditureObjectParameter of Object: string
    | EstimatedCostParameter of EstimatedCost: Money
    | ActualCostParameter of ActualCost: Money
    | SpendDateParameter of SpendDate: DateTime
    | SpendingIdParameter of SpendingId: SpendingId

module CommandParameter =
    let CountParameterName = "count"
    let EstimatedCostParameterName = "estimatedCost"
    let ExpenditureObjectParameterName = "expenditureObject"
    let ActualCostParameterName = "actualCost"
    let SpendDateParameterName = "spendDate"
    let SpendingIdParameterName = "spendingId"
    
    let toParameterName param =
        match param with
        | CountParameter _ -> CountParameterName
        | EstimatedCostParameter _ -> EstimatedCostParameterName
        | ExpenditureObjectParameter _ -> ExpenditureObjectParameterName
        | ActualCostParameter _ -> ActualCostParameterName
        | SpendDateParameter _ -> SpendDateParameterName
        | SpendingIdParameter _ -> SpendingIdParameterName
    
    let (|SpendDateCommandParameter|_|) (name: Token, value: Token) =
        match (name.Type, value.Type) with
        | Word, String ->
            let mutable res = DateTime.Today
            let parsed = DateTime.TryParse(value.Source, &res)
            
            if parsed && name.Source = SpendDateParameterName then
                Some res 
            else
                None            
        | _ -> None
        
    let (|ActualCostCommandParameter|_|) (name: Token, value: Token) =
        match (name.Type, value.Type) with
        | Word, String ->
            let parsedMoney = Money.tryParse value.Source
            match parsedMoney with
            | Some m when name.Source = ActualCostParameterName ->
                Some m
            | _ -> None
        | _ -> None    
            
     
    let (|CountCommandParameter|_|) (name: Token, value: Token) =
        match (name.Type, value.Type) with
        | Word, Number ->
            let mutable res = 0
            let parsed = Int32.TryParse(value.Source, &res)
            
            if parsed && name.Source = CountParameterName then
                Some res 
            else
                None
        | _ -> None        
            
    let (|SpendingIdCommandParameter|_|) (name: Token, value: Token) =
        match (name.Type,  value.Type) with
        | Word, String ->
            let mutable res = Guid.Empty
            let parsed = Guid.TryParse(value.Source, &res)
            
            if parsed && name.Source = SpendingIdParameterName then
                res |> SpendingId |> Some 
            else
                None
        | _ -> None         
    
    let (|EstimatedCostCommandParameter|_|) (name: Token, value: Token) =
        match (name.Type, value.Type) with
        | Word, String ->
            let parsedMoney = Money.tryParse value.Source
            match parsedMoney with
            | Some m when name.Source = EstimatedCostParameterName ->
                Some m
            | _ -> None
        | _ -> None    
    
    let (|ExpenditureObjectCommandParameter|_|) (name: Token, value: Token) =
        match (name.Type, value.Type) with
        | Word, String ->
            if name.Source = ExpenditureObjectParameterName then
                Some value.Source
            else None
        | _ -> None    
            
    let buildParam name value: Result<CommandParameter, Error> =
        match (name, value) with
        | CountCommandParameter i -> CountParameter <| i |> Ok
        | EstimatedCostCommandParameter i -> EstimatedCostParameter <| i |> Ok
        | ExpenditureObjectCommandParameter i -> ExpenditureObjectParameter <| i |> Ok
        | ActualCostCommandParameter i -> ActualCostParameter <| i |> Ok
        | SpendDateCommandParameter i -> SpendDateParameter <| i |> Ok
        | SpendingIdCommandParameter i -> SpendingIdParameter <| i |> Ok
        | _ -> Error (UndefinedParameter  (name.Source, value.Source))
     
    let rec filterShowSpending filters items  =
        match items with
        | Ok l ->
            match filters with
            | param::tail ->
                match param with
                | CountParameter p -> l |> List.safeTake p |> Ok |> filterShowSpending tail
                | SpendDateParameter _
                | ActualCostParameter _
                | SpendingIdParameter _
                | EstimatedCostParameter _
                | ExpenditureObjectParameter _ -> param |> toParameterName |> ExpectedFilterParameter |> Error
            | [] -> items
        | Error e -> Error e   
    
    let continueParameterValue tokens nameToken =
        match tokens with
        | valueToken::tail when valueToken.Type = TokenType.Number || valueToken.Type = TokenType.String ->
            match (nameToken, valueToken) ||> buildParam with
            | Ok p -> (p, tail) |> Ok
            | Error e -> e |> Error
        | valueToken::_ -> valueToken.Position |> UnexpectedParameterValueType |> Error
        | [] -> (None, "No value is specified for the parameter at the end") |> UnfinishedConstruction |> Error
    
    let continueDoubleDot tokens nameToken =
        match tokens with
        | { Type = TokenType.DoubleDot }::tail -> (tail, nameToken) ||> continueParameterValue
        | valueToken::_ -> (valueToken.Position, "The parameter name and value must be separated by a colon") |> UnexpectedToken |> Error
        | [] -> (None, "No value is specified for the parameter at the end") |> UnfinishedConstruction |> Error
         
    let continueParameterName tokens =
        match tokens with
        | t::tail when t.Type = TokenType.Word -> (tail, t) ||> continueDoubleDot
        | valueToken::_ -> (valueToken.Position, "A dash must be followed by the name of the parameter") |> UnexpectedToken |> Error
        | [] -> (None, "No value is specified for the parameter at the end") |> UnfinishedConstruction |> Error
         
    let parseParams (tokens: Token list): Result<CommandParameter list, Error> =
        let rec loop parameters list  =
            match list with
            | { Type = TokenType.Dash }::tail ->
                match tail |> continueParameterName with 
                | Ok (p, t) -> (p::parameters, t) ||> loop
                | Error e -> e |> Error
            | valueToken::_ -> (valueToken.Position, "Parameter must begin with a dash") |> UnexpectedToken |> Error
            | [] -> parameters |> Ok
        
        loop [] tokens