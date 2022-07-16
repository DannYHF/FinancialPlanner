namespace FinancialPlanner

open System.Text
open System.Text.RegularExpressions

type TokenizerState =
    | End
    | String

type Token =
    | Space           // ' '
    | DoubleDot       // ':'
    | Dash            // '-'
    | EnglishLetter   // ^[a-zA-z]$
    | Quote           // '"'
    

module Tokenizer =
    let (|SpaceToken|_|) symbol = if symbol = ' ' then Some () else None
    let (|DoubleDotToken|_|) symbol = if symbol = ':' then Some () else None           
    let (|DashToken|_|) symbol = if symbol = '-' then Some () else None
    let (|QuoteToken|_|) symbol = if symbol = '"' then Some () else None
    let (|EnglishLetterToken|_|) (symbol: char) =
        if (Regex.Match (string <| symbol, "^[a-zA-z]$")).Success then Some () else None 
            
    let tokenize (input: string): string list =
        let builder = StringBuilder()
        let rec loop idx state tokens =
            if idx < input.Length then
                match state, input[idx] with
                | String, QuoteToken ->
                    let str = builder.ToString()
                    builder.Clear () |> ignore
                    loop (idx + 1) TokenizerState.End (str :: tokens)
                | String, curr ->
                    builder.Append curr |> ignore
                    loop (idx + 1) TokenizerState.String tokens
                | End, QuoteToken -> loop (idx + 1) TokenizerState.String tokens
                | End, SpaceToken ->
                    if builder.Length <> 0 then
                        let str = builder.ToString()
                        builder.Clear () |> ignore
                        loop (idx + 1) TokenizerState.End (str :: tokens)
                    else
                        loop (idx + 1) TokenizerState.End tokens
                | End, curr ->
                    builder.Append curr |> ignore
                    loop (idx + 1) TokenizerState.End tokens
            else
                tokens
                
        loop 0 TokenizerState.End []   