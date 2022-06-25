module FinancialPlanner.Domain

open System

type SpendingId = SpendingId of Guid

type Currency =
    { Code: string
      Name: string
      PostFix: char }

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

module Currency =
    let Dollar: Currency =
        { Code = "USD"
          Name = "Доллар"
          PostFix = '$' }

    let Ruble: Currency =
        { Code = "RUB"
          Name = "Рубль"
          PostFix = '₽' }
