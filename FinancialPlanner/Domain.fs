namespace FinancialPlanner.Domain

open System

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
    
type CreateExpectedSpendingForm =
   { ExpenditureObject: string
     EstimatedCost: Money }
       
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
    
    let makeActual (actualCost, spendDate) (expectedSpending: ExpectedSpending) =
        { Id = expectedSpending.Id
          CreationDate = expectedSpending.CreationDate
          ExpenditureObject = expectedSpending.ExpenditureObject
          EstimatedCost = expectedSpending.EstimatedCost
          ActualCost = actualCost
          SpentDate = spendDate }