module Figtree.Recognize
open Figtree.Rule
open Figtree.Match

module Seq =
    let tryTake (n : int) (s : _ seq) =
        seq {
            use e = s.GetEnumerator ()
            let mutable i = 0
            while e.MoveNext () && i < n do
                i <- i + 1
                yield e.Current
        }


// not used here
let isRecursive (rules: RuleTable) (rule: Rule) =
    let rec analyze (visited: Set<string>) (rule: Rule) =
        match rule with
        | Repeat (r, _, _) -> analyze visited r
        | EachOf xs -> 
            xs 
            |> Seq.filter (analyze visited)
            |> Seq.isEmpty
            |> not
        | Ref name -> 
            if Set.contains name visited 
            then true
            else analyze (Set.add name visited) rules.[name]
        | _ -> false
    analyze (set[]) rule

// check if all rules a rule can be expanded to...
let nonZeroRefs (rules: RuleTable) (rule: Rule) =
    let rec isNonEmpty (rule: Rule) =
        match rule with
        | Repeat(r, 0, _) -> false
        | EachOf xs -> 
            xs 
            |> Seq.filter isNonEmpty
            |> Seq.isEmpty
            |> not
        | AnyOf xs -> 
            xs 
            |> Seq.forall isNonEmpty
        | Ref x -> isNonEmpty rules.[x]
        | _ -> true

    let rec analyze (visited: Set<string>) (rule: Rule) : Set<string> =
        match rule with
        | Repeat (r, _, _) -> analyze visited r
        | EachOf xs -> 
            xs 
            |> Seq.filter (isNonEmpty)
            |> Seq.map (analyze visited)
            |> Seq.take 1
            |> Set.unionMany
        | Ref name -> 
            if Set.contains name visited 
            then visited
            else analyze (Set.add name visited) rules.[name]
        | _ -> visited
    analyze (set[]) rule


// hybrid top-down bottom-up parser
// idea: 
// perform a top-down parsing for non-left-recursive rules
// perform a shift-reduce like in bottom-up parsing to expand left-recursive rules
// all rules are greedy
// AnyOf chooses longest sequence
let recognize (rules: RuleTable) (current: string) (str: string) (i: int) =
    let rec applyRule (path: List<string*int>) rule j =
        match str.Length > j,  rule with
        | false, _ -> None
        | _, CharSet xs -> 
            if Set.contains str.[j] xs
            then Some <| Leaf (path, j, j+1)
            else None
        | _, CharRange (a, b) -> 
            if a <= str.[j] && str.[j] <= b
            then 
                Some <| Leaf (path, j, j+1)
            else None
        | _, Repeat (rule, a, b) ->
            let rec loop j = seq {
                match applyRule path rule j with
                | Some x ->
                    yield x
                    yield! loop (getEnd x)
                | None -> ()
            }
            let xs =
                if b>0 
                then loop j |> Seq.tryTake(b)
                else loop j
                |> Seq.toList
            match xs |> List.length >= a  with
            | true -> 
                let e = xs |> List.last |> getEnd
                Some (Tree (path, j, e, xs))
            | false -> None
        | _, AnyOf xs ->
            xs 
            |> Seq.choose (fun rule -> applyRule path rule j) 
            |> Seq.sortByDescending getEnd
            |> Seq.tryHead
        | _, EachOf todo ->
            let rec reduce j todo = seq {
                match todo with
                | [] -> ()
                | x::xs ->
                    match applyRule path x j with
                    | None -> ()
                    | Some x ->
                        yield x
                        yield! reduce (getEnd x) xs
            }

            // apply each rule one by one
            // return iif all rules sucessfull
            let runRule j todo =
                let result = reduce j todo |> Seq.toList
                match result.Length = todo.Length with
                | true ->
                    let e = result |> Seq.last |> getEnd  
                    Some <| result
                | false -> None

            // bottom-up solution to left-recursion 
            // we check if the first rule is a recursion
            //   if yes then we check if we can expand the current match to the recursive rule
            //     if yes then we skip that rule and the current match assumes it's possition
            // rule table:
            // a : x
            // x : a b c <- we analyze this
            // if we parsed sucessfully x then check if x can be expanded to a (reduce)
            // if yes then proceed with parsing b and c (shift)
            // TODO: don't shiftReduce if inside Repeat rule
            // left-recursion has priority over right-recursion
            let rec shiftReduce result =
                match result with
                | None -> None
                | Some results ->
                    let e = 
                        results
                        |> Seq.last
                        |> getEnd
                    match todo with
                    | (Ref x)::xs -> 
                        // check all the rules that rule (Ref x) can expand to
                        // if so then shiftreduce
                        let alias = nonZeroRefs rules (Ref x)
                        let shouldShiftReduce = 
                            path 
                            |> Seq.map fst 
                            |> Seq.tryFind (fun x -> Set.contains x alias)
                            |> Option.isSome
                        
                        match shouldShiftReduce with
                        | true ->
                            match runRule e xs with
                            | None -> Some <| Tree (path, j, e, results)
                            | Some x -> 
                                let e = 
                                    x
                                    |> Seq.last
                                    |> getEnd
                                shiftReduce (Some[(Tree (path, j, e, results @ x))])
                        | _ -> Some <| Tree (path, j, e, results)
                    | [] -> Some <| Tree (path, j, e, results)
                    | _ -> Some <| Tree (path, j, e, results)
            
            let result = runRule j todo
            shiftReduce result

        // potential recursion, we mark the name to detect cycles during left-recursion
        // the name will carried forward in the AST (but we should add a name to more primitive rules too!)
        | _, Ref name ->
            let rule = rules.[name]
            match List.contains (name, j) path with
            | true -> None
            | false ->
                let next = (name, j) :: path
                match applyRule next rule j with
                | Some x -> 
                    let p = getPath x
                    let e = getEnd x
                    Some <| Tree (path, j, e, [x])
                | None -> None
    applyRule ([current, -1]) rules.[current] i




// type ParseAction = 
// | Transition of string
// | Stay
// | Pop of string


// type StateTransitions = Map<string, Map<Rule, ParseAction>>


// let analyze (transitions: StateTransitions) (rules : RuleTable) (rule: Rule) (name: string) =
//     match rule with
//     | CharRange (a, b) ->
//         transitions 
//         |> Map.add
//             name 
//             (transitions
//             |> Map.tryFind name 
//             |> Option.defaultValue Map.empty
//             |> Map.add rule (Pop name))
//     | CharSet xs ->
//         transitions 
//         |> Map.add
//             name 
//             (transitions
//             |> Map.tryFind name 
//             |> Option.defaultValue Map.empty
//             |> Map.add rule (Pop name))
//     | CharSet xs ->
//         transitions 
//         |> Map.add
//             name 
//             (transitions
//             |> Map.tryFind name 
//             |> Option.defaultValue Map.empty
//             |> Map.add rule (Pop name))
        