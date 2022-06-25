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
      EstimatedAmountOfMoney: Money }

type ActualSpending =
    { Id: SpendingId
      CreationDate: DateTime

      ExpenditureObject: string
      EstimatedAmountOfMoney: Money

      ActualMoneySpent: Money
      SpentDate: DateTime }

type Spending =
    | Actual of ActualSpending
    | Expected of ExpectedSpending

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
                 EstimatedAmountOfMoney = a.EstimatedAmountOfMoney
                 ActualMoneySpent = a.ActualMoneySpent
                 SpentDate = a.SpentDate }
        | Expected e ->
            Expected
            <| { Id = id
                 CreationDate = e.CreationDate
                 ExpenditureObject = e.ExpenditureObject
                 EstimatedAmountOfMoney = e.EstimatedAmountOfMoney }

module Currency =
    let Dollar: Currency =
        { Code = "USD"
          Name = "Доллар"
          PostFix = '$' }

    let Ruble: Currency =
        { Code = "RUB"
          Name = "Рубль"
          PostFix = '₽' }
