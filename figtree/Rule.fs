module Figtree.Rule

type Rule =
| Repeat of Rule * int * int
| CharRange of char * char
| CharSet of Set<char>
| AnyOf of List<Rule>
| EachOf of List<Rule>
| Ref of string

type UnverifiedRuleTable = Map<string, Rule>

let ruleTable (rules : seq<string * Rule>) =
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

[<Struct>]
type RuleTable =
  private
  | RuleTable of Map<string, Rule>

type RuleVerificationError =
  | UndefinedReference of rule : string * ref : string

module RuleTable =

  let rules =
    function
    | RuleTable m -> m

  let verify (x : UnverifiedRuleTable) : Result<RuleTable, RuleVerificationError list> =
    let rec undefinedReferences rule =
      seq {
        match rule with
        | Repeat (r, _, _) ->
          yield! undefinedReferences r
        | CharRange _ -> ()
        | CharSet _ -> ()
        | AnyOf xs ->
          yield! Seq.collect undefinedReferences xs
        | EachOf xs ->
          yield! Seq.collect undefinedReferences xs
        | Ref r ->
          if Map.containsKey r x |> not
          then
            yield r
      }

    let errors =
      seq {
        for (k, v) in x |> Map.toSeq do
          yield!
            undefinedReferences v
            |> Seq.map (fun r -> UndefinedReference (k, r))
      }
      |> Seq.toList

    if Seq.isEmpty errors
    then
      Ok (RuleTable x)
    else
      Error errors
