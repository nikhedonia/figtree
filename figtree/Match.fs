module Figtree.Match

type Match =
| Leaf of List<string * int> * int * int 
| Tree of List<string * int> * int * int * List<Match> 

let getEnd (m: Match) =
    match m with
    | Leaf (_, _, x) -> x
    | Tree (_, _, x, _) -> x

let getPath (m: Match) =
    match m with
    | Leaf (p, _, _) -> p
    | Tree (p, _, _, _) -> p

let prettyPrint (m: Match) (str: string)=
    let rec print (m: Match) (indent: int) =
        let space = String.replicate indent " "
        match m with
        | Tree (path, s, e, xs) ->
            let head = path |> List.head |> fst
            System.Console.WriteLine 
                (space + (string s) + " " + (string e) + " " + head + " -> '" + str.Substring(s, e-s) + "'")
            for x in xs do
                print x (indent + 4)
        | Leaf (path, s, e) ->
            let head = path |> List.head |> fst
            System.Console.WriteLine 
                (space + (string s) + " " + (string e) + " " + head + " -> '" + str.Substring(s, e-s) + "'")
    print m 0