namespace FinancialPlanner.Domain

open System
open FsToolkit.ErrorHandling
open Microsoft.FSharp.Core

type SpendingId = SpendingId of Guid

type Currency =
    { Code: string
      Name: string
      PostFix: char}

type Money = { Amount: decimal; Currency: Currency }

type ExpectedSpending =
    { Id: SpendingId
      CreationDate: DateTime

      ExpenditureObject: string
      EstimatedCost: decimal
      Currency: Currency }

type ActualSpending =
    { Id: SpendingId
      CreationDate: DateTime

      ExpenditureObject: string
      EstimatedCost: decimal
      Currency: Currency

      ActualSpent: decimal
      SpentDate: DateTime }

type Spending =
    | Actual of ActualSpending
    | Expected of ExpectedSpending
    
type CreateExpectedSpendingForm =
   { ExpenditureObject: string
     EstimatedCost: Money }
       
type ShortSpendingsStatistics =
    { TotalSpent: decimal
      StillExpectedToSpend: decimal
      DifferenceBetweenPlannedAndSpent: decimal
      Currency: Currency}
    
module Currency =
    let Dollar: Currency =
        { Code = "USD"
          Name = "Доллар"
          PostFix = '$' }
    
    let DollarSupportedPostFixes = [ '$'; 'д'; 'd' ]     

    let Ruble: Currency =
        { Code = "RUB"
          Name = "Рубль"
          PostFix = '₽' }
    
    let RubleSupportedPostFixes = [ '₽'; 'р'; 'r' ]    

[<RequireQualifiedAccess>]        
module Money =
    let tryParse (money:string) =
        let mutable value = Decimal.Zero
        let postFix = money.[money.Length - 1]
        let strValue = money.Substring (0, money.Length - 1)
        if Decimal.TryParse (strValue, &value) then
            match postFix with
            | p when Currency.DollarSupportedPostFixes |> List.contains p ->
                    { Amount = value
                      Currency = Currency.Dollar } |> Some
            | p when Currency.RubleSupportedPostFixes |> List.contains p ->
                    { Amount = value
                      Currency = Currency.Ruble } |> Some                    
            | _ -> None
        else
            None

module Spending =
    let getCurrency spending =
        match spending with
        | Actual a -> a.Currency
        | Expected e -> e.Currency
        
    let getId spending =
        match spending with
        | Actual a -> a.Id
        | Expected e -> e.Id

    let setId id spending: Spending =
        match spending with
        | Actual a ->
          Actual
            <| { Id = id
                 CreationDate = a.CreationDate
                 ExpenditureObject = a.ExpenditureObject
                 EstimatedCost = a.EstimatedCost
                 ActualSpent = a.ActualSpent
                 SpentDate = a.SpentDate
                 Currency = a.Currency }
        | Expected e ->
            Expected
            <| { Id = id
                 CreationDate = e.CreationDate
                 ExpenditureObject = e.ExpenditureObject
                 EstimatedCost = e.EstimatedCost
                 Currency = e.Currency }

    let createExpected form =
        { Id = SpendingId (Guid.NewGuid ())
          CreationDate = DateTime.Now
          ExpenditureObject = form.ExpenditureObject
          EstimatedCost = form.EstimatedCost.Amount
          Currency = form.EstimatedCost.Currency }
    
    let makeActual (actualSpent, spendDate) (expectedSpending: ExpectedSpending) =
        { Id = expectedSpending.Id
          CreationDate = expectedSpending.CreationDate
          ExpenditureObject = expectedSpending.ExpenditureObject
          EstimatedCost = expectedSpending.EstimatedCost
          ActualSpent = actualSpent
          SpentDate = spendDate
          Currency = expectedSpending.Currency }
    
    let prepareShortStatistics spendings =
        let groupedByCurrency = spendings |> List.groupBy (fun u -> u |> getCurrency)
        groupedByCurrency |> List.map (fun (currency, spendings) ->
            { TotalSpent = (Decimal.Zero, spendings) ||> List.fold (fun acc s ->
                match s with
                | Actual a -> acc + a.ActualSpent
                | Expected _ -> acc)
              StillExpectedToSpend = (Decimal.Zero, spendings) ||> List.fold (fun acc s ->
                match s with
                | Expected e -> e.EstimatedCost + acc
                | Actual _ -> acc) 
              DifferenceBetweenPlannedAndSpent = (Decimal.Zero, spendings) ||> List.fold (fun acc s ->
                match s with
                | Actual a -> acc + a.EstimatedCost - a.ActualSpent
                | Expected _ -> acc)
              Currency = currency })