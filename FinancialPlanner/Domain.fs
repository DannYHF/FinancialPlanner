module FinancialPlanner.Domain

open System

type SpendingId = SpendingId of Guid

type Currency =
    { Code: string
      Name: string
      MainPostFix: char
      SupportedPostFixes: char list }

type Money = { Amount: decimal; Currency: Currency }

type ExpectedSpending =
    { Id: SpendingId
      CreationDate: DateTime

      ExpenditureObject: string
      EstimatedCost: Money }

type ActualSpending =
    { Id: SpendingId
      CreationDate: DateTime

      ExpenditureObject: string
      EstimatedCost: Money

      ActualCost: Money
      SpentDate: DateTime }

type Spending =
    | Actual of ActualSpending
    | Expected of ExpectedSpending
    
module Currency =
    let Dollar: Currency =
        { Code = "USD"
          Name = "Доллар"
          MainPostFix = '$'
          SupportedPostFixes = [ '$'; 'д' ] }

    let Ruble: Currency =
        { Code = "RUB"
          Name = "Рубль"
          MainPostFix = '₽'
          SupportedPostFixes = [ '₽'; 'р' ] }

[<RequireQualifiedAccess>]        
module Money =
    let tryParse (money:string) =
        let mutable value = Decimal.Zero
        let postFix = money.[money.Length - 1]
        let strValue = money.Substring (0, money.Length - 1)
        if Decimal.TryParse (strValue, &value) then
            match postFix with
            | p when Currency.Dollar.SupportedPostFixes |> List.contains p ->
                    { Amount = value
                      Currency = Currency.Dollar } |> Some
            | p when Currency.Ruble.SupportedPostFixes |> List.contains p ->
                    { Amount = value
                      Currency = Currency.Ruble } |> Some                    
            | _ -> None
        else
            None

module Spending =
    type CreateExpectedSpendingForm =
        { ExpenditureObject: string
          EstimatedCost: Money }    

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
                 ActualCost = a.ActualCost
                 SpentDate = a.SpentDate }
        | Expected e ->
            Expected
            <| { Id = id
                 CreationDate = e.CreationDate
                 ExpenditureObject = e.ExpenditureObject
                 EstimatedCost = e.EstimatedCost }
    
    let createExpected form =
        { Id = SpendingId (Guid.NewGuid ())
          CreationDate = DateTime.Now
          ExpenditureObject = form.ExpenditureObject
          EstimatedCost = form.EstimatedCost}