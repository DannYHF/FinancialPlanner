namespace FinancialPlanner

open System
open System.Text
open System.Text.RegularExpressions
open FinancialPlanner.Error



type TokenType =
    | Space           // ' '
    | DoubleDot       // ':'
    | Dash            // '-'
    | EnglishLetter   // ^[a-zA-z]$
    | Quote           // '"'
    | Number          // ^[0-9]$
    | Word            // 'someword'
    | String          // 'some sybmols 123%?'
    
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
    let (|NumberToken|_|) (symbol: char) = 
        if symbol |> Char.IsDigit then Some Number else None
        
        
    let tokenizeWord (str: string) position tokens =
        let builder = StringBuilder()
        let rec loop idx =
            if idx < str.Length then
                let sym = str[idx]
                match sym with
                | EnglishLetterToken _ ->
                    builder.Append(sym) |> ignore
                    loop (idx + 1)
                | _ -> (idx, { Source = builder.ToString(); Position = idx; Type = Word } :: tokens)    
            else
                (idx, { Source = builder.ToString(); Position = idx; Type = Word } :: tokens)
        loop position

    let tokenizeNumber (str: string) position tokens =
        let builder = StringBuilder()
        let rec loop idx =
            if idx < str.Length then
                let sym = str[idx]
                match sym with
                | NumberToken _ ->
                    builder.Append(sym) |> ignore
                    loop (idx + 1)
                | _ -> (idx, { Source = builder.ToString(); Position = idx; Type = Number } :: tokens)    
            else
                (idx, { Source = builder.ToString(); Position = idx; Type = Number } :: tokens)
        loop position
        
    let tokenizeString (str: string) position tokens =
        let builder = StringBuilder(String.Empty)
        let rec loop idx =
            if idx < str.Length then
                let sym = str[idx]
                match sym with
                | QuoteToken _ -> (idx + 1, { Source = builder.ToString(); Position = idx; Type = String } :: tokens) |> Ok
                | _ -> builder.Append(sym) |> ignore
                       loop (idx + 1) 
            else
                (Some idx, "Expected '\"' in end of the string.") |> UnfinishedConstruction |> Error 
        loop position          
       
    let rec tokenize (input: string): Result<Token list, Error> =
        let rec loop idx tokens: Result<Token list, Error> =
            if idx < input.Length then
                let sym = input[idx]
                match sym with
                | SpaceToken _ -> loop (idx + 1) tokens
                | DoubleDotToken t -> loop (idx + 1) ({ Source = sym.ToString(); Position = idx; Type = t } :: tokens)
                | DashToken t -> loop (idx + 1) ({ Source = sym.ToString(); Position = idx; Type = t } :: tokens)
                | EnglishLetterToken _ -> (input, idx, tokens) |||> tokenizeWord ||> loop
                | NumberToken _ -> (input, idx, tokens) |||> tokenizeNumber ||> loop
                | QuoteToken _ ->
                    let res = (input, idx + 1, tokens) |||> tokenizeString
                    match res with
                    | Ok r -> r ||> loop
                    | Error e -> e |> Error 
                | _ -> (idx, sym) |> UndefinedSymbol |> Error 
            else tokens |> List.rev |> Ok
        loop 0 []