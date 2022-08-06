namespace FinancialPlanner.Error

type Error =
    | ParsingFailed of string
    | UndefinedParameter of parameterName: string * parameterValue: string
    | UndefinedCommand of commandName: string
    | NotSuitableParameter of commandName: string * parameterName: string
    | ExpectedFilterParameter of parameterName: string
    | MandatoryParametersAreNotFilled of requiredParameters: string list
    | UndefinedSymbol of position: int * token: char
    | UnfinishedConstruction of position: int * message: string