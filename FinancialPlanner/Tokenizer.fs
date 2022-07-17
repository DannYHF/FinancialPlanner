namespace FinancialPlanner

open System.Text
open System.Text.RegularExpressions
open FinancialPlanner.Error

type TokenizerState =
    | End
    | String

type TokenType =
    | Space           // ' '
    | DoubleDot       // ':'
    | Dash            // '-'
    | EnglishLetter   // ^[a-zA-z]$
    | Quote           // '"'

type Token =
    { Source: string
      Position: int
      Type: TokenType }

module Tokenizer =
    let (|SpaceToken|_|) symbol = if symbol = ' ' then Some Space else None
    let (|DoubleDotToken|_|) symbol = if symbol = ':' then Some DoubleDot else None           
    let (|DashToken|_|) symbol = if symbol = '-' then Some Dash else None
    let (|QuoteToken|_|) symbol = if symbol = '"' then Some Quote else None
    let (|EnglishLetterToken|_|) (symbol: char) =
        if (Regex.Match (string <| symbol, "^[a-zA-z]$")).Success then Some EnglishLetter else None 
            
    let tokenize (input: string) =
        let builder = StringBuilder()
        let rec loop idx state tokens =
            if idx < input.Length then
                let curr = input[idx]
                match state, curr with
                | String, QuoteToken t ->
                    let token = { Source = builder.ToString()
                                  Position = idx
                                  Type = t } 
                    builder.Clear () |> ignore
                    loop (idx + 1) TokenizerState.End (token :: tokens)
                | String, c ->
                    builder.Append c |> ignore
                    loop (idx + 1) TokenizerState.String tokens
                | End, QuoteToken _ -> loop (idx + 1) TokenizerState.String tokens
                | End, SpaceToken t ->
                    if builder.Length <> 0 then
                        let token = { Source = builder.ToString()
                                      Position = idx
                                      Type = t }
                        builder.Clear () |> ignore
                        loop (idx + 1) TokenizerState.End (token :: tokens)
                    else
                        loop (idx + 1) TokenizerState.End tokens
                | End, EnglishLetterToken _
                | End, DashToken _
                | End, DoubleDotToken _ ->
                    builder.Append curr |> ignore
                    loop (idx + 1) TokenizerState.End tokens
                | End, _ -> (idx, curr) |> UndefinedSymbol |> Error   
            else
                tokens |> Ok
                
        loop 0 TokenizerState.End []   