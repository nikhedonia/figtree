module Figtree.Combinator
type View = {
    Start: int
    End: int
}

let start = {Start = 0; End = 0}

type CacheAction<'T> =
| Fail of View
| Recurse of int
| Complete of View * 'T
and State<'T> = {
    Value: 'T
    View: View
    Cache: Cache
} and Cache = Map<int*int, CacheAction<obj>>
and ParseResult<'T> =
| Success of State<'T>
| Partial of Set<int> * State<'T>
| Recursion of Set<int> * State<Unit> 
| Error of State<Unit>
and Parser<'T> = State<Unit> -> string -> ParseResult<'T>


type ParserCombinator<'T, 'U> = 'T -> Parser<'U>

let zero = {
  Value = ()
  View = {Start=0; End=0}
  Cache = Map.empty
}

module Parser =
  let strip (s: State<_>): State<Unit> = {
    Value = ()
    View = s.View
    Cache = s.Cache
  }

  let rec getState (s: ParseResult<'T>) =
    match s with
    | Success x -> strip x
    | Recursion (_, x) -> x
    | Partial (_, x) -> strip x
    | Error x -> x

  let rec getCache (s: ParseResult<'T>) =
    match s with
    | Success x -> x.Cache
    | Recursion (_, x) -> x.Cache
    | Partial (_, x) -> x.Cache
    | Error x -> x.Cache

  let rec updateCache (u: Cache -> Cache) (s: ParseResult<'T>) =
    match s with
    | Success x -> Success { x with Cache = u x.Cache }
    | Recursion (ids, x) -> Recursion (ids, { x with Cache = u x.Cache })
    | Partial (ids, x) -> Partial (ids, { x with Cache = u x.Cache })
    | Error x -> Error { x with Cache = u x.Cache }

  let setStart (start: int) (r: ParseResult<'T>) =
      match r with
      | Success s -> Success {s with View = {s.View with Start = start}}
      | Error s -> Error {s with View = {s.View with Start = start}}
      | Partial (fid, s) -> Partial (fid, {s with View = {s.View with Start = start}})
      | Recursion (x, s) -> Recursion (x, {s with View = {s.View with Start = start}})


  let score x =
    match x with 
    | Success s -> (3, s.View.End)
    | Partial (id, s) -> (2, s.View.End)
    | Recursion (id, s) -> (1, s.View.End)
    | Error s -> (0, s.View.End)

  let runParser (p: Parser<'T>) (state: State<Unit>) (str: string) =
    p state str

  let rec parseAt ( p: Parser<'T>) (x: State<_>) (str: string) =
    p x str

  let parse ( p: Parser<'T>) (str: string) = 
    match parseAt p zero str with
    | Partial (id, x) ->
        if x.View.End = str.Length
        then Success x
        else Partial (id, x)
    | x -> x

  let rec Bind (p: Parser<'T>) (pc: ParserCombinator<'T, 'U>): Parser<'U>  =
    let run (x: State<Unit>) (str: string): ParseResult<'U> =
      match runParser p x str with
      | Error x -> Error x
      | Success next -> runParser (pc next.Value) (strip next) str
      | Recursion (a, b) -> Recursion (a, b)
      | Partial (id, next) ->
        match runParser (pc next.Value) (strip next) str with
        | Success x -> Partial (id, x)
        | Partial (id2, x) -> Partial (Set.union id id2, x) 
        | Error x -> Error x
        | Recursion (a, b) -> Recursion (a, b)
            
    run

  let Return (value: 'T): Parser<'T> =
    let run (x: State<Unit>) (str: string) =
        Success {
            Value = value
            View = x.View
            Cache = x.Cache
        }
          
    run

type Parse (cacheKey: obj) =
  let id = hash(cacheKey)

  // Delay wraps the assembled Computation Expression in a function 
  // This inject caching and a bottom up parse step for left-recursive combinators
  member this.Delay (f: ParserCombinator<Unit, 'T>) =
    let rec updateCache (state: State<Unit>) (result: ParseResult<'T>) =
      let e = state.View.End
      match result |> Parser.setStart state.View.Start with
      | Success s -> Success {s with Cache = s.Cache |> Map.add (id, e) (Complete (s.View, s.Value :> obj))}
      | Error s -> Error {s with Cache = s.Cache |> Map.add (id, e) (Fail s.View)}
      | Partial (fid, s) -> Partial (fid, {s with Cache = s.Cache |> Map.add (id, e) (Complete (s.View, s.Value :> obj))})
      | Recursion (x, y) -> Recursion (x, y)
    
    let delay (state: State<Unit>) (str: string) =
        let e = state.View.End
        match state.Cache |> Map.tryFind (id, e) with
        | Some (Fail view) -> Error {state with View = view}
        | Some (Recurse id) ->
            // Recursion detected, don't continue computation
            Recursion (set[id], state)
        | Some (Complete (v, value)) -> Success {
              Value =  value  :?> 'T
              View = v
              Cache = state.Cache
            }
        | None -> 
            let rec shiftReduce next prev c =
                match Parser.runParser (f()) next str |> updateCache state with
                | Success s -> Success s
                | Error _ -> prev
                | Recursion (fid, state) -> Recursion (Set.add id fid, state)
                | Partial (fid, state) when not(Set.contains id fid) -> 
                    Partial (fid, state)
                | Partial (_, result) ->
                    let r = Success result
                    shiftReduce {Parser.getState r with View = state.View} r (c+1)
 
            let next = {
                state with 
                    Cache = Map.add (id, e) (Recurse id) state.Cache}

            shiftReduce next (Error next) 1
                  

    delay

            
        
  member this.Return (value: 'T) = Parser.Return value
  member this.ReturnFrom (p: Parser<'T>) = p
  member this.Bind (p: Parser<'T>, pc: ParserCombinator<'T, 'U>): Parser<'U> =
      Parser.Bind p pc
        
// TODO: return/apply Rule instead of operating on strings directly
let parseString (x: string) =
  let f (s: State<Unit>) (str: string) = 
    let fail = (str.Length - s.View.End) < x.Length
    if not fail && str.Substring(s.View.End, x.Length) = x
    then Success {
        Value = x
        Cache = s.Cache
        View = {
          Start = s.View.End
          End = s.View.End + x.Length
        }
      }
    else Error {
        Value = ()
        Cache = s.Cache
        View = {
          Start = s.View.Start
          End = s.View.Start
        }
      }
  f


let private isRecursion (k, v) =
  match v with
  | Recurse _ -> true
  | _ -> false

let private dropNewRecursions old k v =
  match v with
  | Recurse _ -> Set.contains k old 
  | _ -> true


let rec bestOf (xs: List<Parser<'T>>): Parser<'T> =

  let run (state: State<Unit>) (str: string) = 
    let removeRecursionNodesOfAlternatives = 
      state.Cache 
      |> Map.toSeq
      |> Seq.filter isRecursion
      |> Seq.map fst
      |> set
      |> dropNewRecursions
      |> Map.filter
      |> Parser.updateCache
    
    let ys = 
      xs // investigate all alternatives and populate cache
      |> Seq.scan (fun (s: ParseResult<'T>) (p: Parser<'T>) -> 
          let next = {
              Parser.getState s 
              with View = state.View // reset the cursor
          } 
          Parser.parseAt p next str 
          |> removeRecursionNodesOfAlternatives
      ) (Error state) // return error if xs is empty
      |> Seq.sortByDescending(fun x -> (Parser.getState x).View.End)
      |> Seq.toList

    let recursive = ys |> Seq.filter(function | Recursion _ -> true | _ -> false) 
    let partials = ys |> Seq.filter(function | Partial _ -> true | _ -> false)

    // list of recursive parsers
    // if recursion is detected then we only have a partial result
    let ids =
      (Seq.concat [
          recursive
          partials
      ])
      |> Seq.choose (function | Recursion (ids,_) -> Some (ids) | Partial (ids, _) -> Some ids | _ -> None)
      |> Set.unionMany

    let complete = 
      ys 
      |> List.choose
        (function 
        | Success x -> 
          Some
            (if ids |> Set.count > 0 
             then Partial (ids, x)
             else Success x)
        | _ -> None)

    let errors = 
      ys 
      |> List.filter(function Error _ -> true | _ -> false)

    Seq.concat [
      complete
      errors
    ] 
    |> Seq.sortByDescending Parser.score
    |> Seq.head
    
  run
    