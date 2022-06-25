module FinancialPlanner.Helper

let getErrors items =
    items
    |> List.choose
        (fun x ->
            match x with
            | Ok _ -> None
            | Error e -> Some e)

let getResults items =
    items
    |> List.choose
        (fun x ->
            match x with
            | Ok i -> Some i
            | Error _ -> None)
        
