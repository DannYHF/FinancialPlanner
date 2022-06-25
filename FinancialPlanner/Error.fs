module FinancialPlanner.Error

type CommandError =
    | ParsingFailed of string
    | UndefinedParameter of parameterName: string * parameterValue: string
    | UndefinedCommand of commandName: string
    | NotSuitableParameter of commandName: string * parameterName: string
    