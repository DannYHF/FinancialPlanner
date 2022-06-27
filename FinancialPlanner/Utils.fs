namespace FinancialPlanner.Utils

module Result =
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
            

module List =
   let safeTake count (list: 'a list) =
       if count >= list.Length then
           list
       else
           list |> List.take count 