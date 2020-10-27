namespace Tests

module Result =

  let get =
    function
    | Ok x -> x
    | Error e -> failwithf "Expected Ok _ but got Error %A" e
