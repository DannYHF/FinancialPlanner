module FinancialPlanner.Error

type CommandError =
    | ParsingFailed
    | UndefinedParameter
    | UndefinedCommand