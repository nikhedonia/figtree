module Figtree.Rule

type Rule =
| Repeat of Rule * int * int
| CharRange of char * char
| CharSet of Set<char>
| AnyOf of List<Rule>
| EachOf of List<Rule>
| Ref of string

type RuleTable = Map<string, Rule> 

let ruleTable (rules: seq<string * Rule>) =
    rules
    |> Seq.toList 
    |> Seq.groupBy fst
    |> Seq.choose (fun (name, rules) -> 
        match rules |> Seq.toList |> List.map snd  with
        | [] -> None
        | [x] -> Some (name, x) 
        | x::xs -> Some (name, AnyOf (x::xs))
    )
    |> Map.ofSeq

