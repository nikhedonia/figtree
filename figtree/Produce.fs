module Figtree.Produce

open Figtree.Rule

let private csub (a : char) (b : char) = int a - int b

let private cadd (a : char) (b : int) = char ((int a) + b)

let produce (rt : RuleTable) (name : string) =
  let g = System.Random()

  let rec loop (rule : Rule) =
    match rule with
    | CharSet xs ->
      let i = g.Next() % Set.count xs

      xs
      |> Set.toSeq
      |> Seq.skip i
      |> Seq.take 1
      |> Seq.map string
      |> String.concat ""

    | CharRange(a, b) ->
      let i = g.Next() % (csub b a)
      cadd a i |> string

    | Repeat(next, a, b) ->
      let maxReps =
        if b > 0 then
          min (b - a + 1) 10
        else
          10

      let c = a + (g.Next() % maxReps)

      (Seq.map ((fun _ -> loop next) >> string) (Seq.replicate c 0))
      |> String.concat ""

    | AnyOf xs ->
      let i = (g.Next() % List.length xs)

      xs
      |> List.skip i
      |> List.take 1
      |> List.map loop
      |> String.concat ""

    | EachOf xs -> xs |> List.map loop |> String.concat ""

    | Ref name ->
      let rule = (RuleTable.rules rt).[name]
      loop rule

  let rule = (RuleTable.rules rt).[name]

  loop rule
