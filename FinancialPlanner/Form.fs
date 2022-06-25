module FinancialPlanner.Form

open FinancialPlanner.Domain

type CreateExpectedSpendingForm =
    { ExpenditureObject: string
      EstimatedCost: Money }    