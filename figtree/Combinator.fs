module Figtree.Combinator

type View =
  {
    Start : int
    End : int
  }

  static member Zero =
    {
      Start = 0
      End = 0
    }

type CacheAction<'T, 'E> =
  | Fail of View * 'E
  | Recurse of int
  | Complete of View * 'T

and State<'T> =
  {
    Value : 'T
    View : View
    Cache : Cache
    Path : List<int>
  } // list of obj?

and Cache = Map<int * int, CacheAction<obj, obj>>

and ParseResult<'T, 'E> =
  | Success of State<'T>
  | Partial of Set<int> * State<'T>
  | Recursion of Set<int> * State<Unit>
  | Error of State<'E>

and Parser<'T, 'E> = State<Unit> -> string -> ParseResult<'T, 'E>


type ParserCombinator<'T, 'U, 'E> = 'T -> Parser<'U, 'E>

let zero =
  {
    Value = ()
    View =
      {
        Start = 0
        End = 0
      }
    Cache = Map.empty
    Path = []
  }

module Parser =
  let strip (s : State<_>) : State<Unit> =
    {
      Value = ()
      View = s.View
      Cache = s.Cache
      Path = s.Path
    }

  let rec getState (s : ParseResult<'T, 'E>) =
    match s with
    | Success x -> strip x
    | Recursion(_, x) -> x
    | Partial(_, x) -> strip x
    | Error x -> strip x

  let rec getCache (s : ParseResult<'T, 'E>) =
    match s with
    | Success x -> x.Cache
    | Recursion(_, x) -> x.Cache
    | Partial(_, x) -> x.Cache
    | Error x -> x.Cache

  let rec updateCache (u : Cache -> Cache) (s : ParseResult<'T, 'E>) =
    match s with
    | Success x -> Success { x with Cache = u x.Cache }
    | Recursion(ids, x) -> Recursion(ids, { x with Cache = u x.Cache })
    | Partial(ids, x) -> Partial(ids, { x with Cache = u x.Cache })
    | Error x -> Error { x with Cache = u x.Cache }

  let setStart (start : int) (r : ParseResult<'T, 'E>) =
    match r with
    | Success s -> Success { s with View = { s.View with Start = start } }
    | Error s -> Error { s with View = { s.View with Start = start } }
    | Partial(fid, s) -> Partial(fid, { s with View = { s.View with Start = start } })
    | Recursion(x, s) -> Recursion(x, { s with View = { s.View with Start = start } })


  let score x =
    match x with
    | Success s -> (3, s.View.End)
    | Partial(id, s) -> (2, s.View.End)
    | Recursion(id, s) -> (1, s.View.End)
    | Error s -> (0, s.View.End)

  let runParser (p : Parser<'T, 'E>) (state : State<Unit>) (str : string) = p state str

  let rec parseAt (p : Parser<'T, 'E>) (x : State<_>) (str : string) = p x str

  let parse (p : Parser<'T, 'E>) (str : string) =
    match parseAt p zero str with
    | Partial(id, x) ->
      if x.View.End = str.Length then
        Success x
      else
        Partial(id, x)
    | x -> x

  let makePartial ids =
    function
    | Success next -> Partial(ids, next)
    | Error e -> Error e
    | Recursion(ids2, state) -> Recursion(Set.union ids ids2, state)
    | Partial(ids2, state) -> Partial(Set.union ids ids2, state)

  let rec Bind
    (p : Parser<'T, 'E>)
    (pc : ParserCombinator<'T, 'U, 'E>)
    (x : State<Unit>)
    (str : string)
    : ParseResult<'U, 'E> =
    match runParser p x str with
    | Error x -> Error x
    | Success next -> runParser (pc next.Value) (strip next) str
    | Recursion(a, b) -> Recursion(a, b)
    | Partial(ids, next) ->
      runParser (pc next.Value) (strip next) str
      |> makePartial ids

  let Return (value : 'T) (x : State<Unit>) (str : string) =
    Success
      {
        Value = value
        View = x.View
        Cache = x.Cache
        Path = x.Path
      }

  let While (cond : Unit -> bool) (f : Parser<'T, 'E>) : Parser<Unit, 'E> =
    let rec loop (state : State<Unit>) (str : string) =
      if cond () then
        match f state str with
        | Success next -> loop (strip next) str
        | Partial(ids, next) -> loop (strip next) str |> makePartial ids
        | Recursion(a, b) -> Recursion(a, b)
        | Error x -> Error x
      else
        Success state

    loop

  let map (f : 'T -> 'U) (p : Parser<'T, 'E>) (state : State<Unit>) (str : string) : ParseResult<'U, 'E> =
    match p state str with
    | Success s ->
      Success
        {
          Value = f s.Value
          Cache = s.Cache
          View = s.View
          Path = s.Path
        }
    | Partial(ids, s) ->
      Partial(
        ids,
        {
          Value = f s.Value
          Cache = s.Cache
          View = s.View
          Path = s.Path
        }
      )
    | Error e -> Error e
    | Recursion(a, b) -> Recursion(a, b)

  let mapError (f : 'E -> 'U) (p : Parser<'T, 'E>) (state : State<Unit>) (str : string) : ParseResult<'T, 'U> =
    match p state str with
    | Success s -> Success s
    | Partial(ids, s) -> Partial(ids, s)
    | Error e ->
      Error
        {
          Value = f e.Value
          Cache = e.Cache
          View = e.View
          Path = e.Path
        }
    | Recursion(a, b) -> Recursion(a, b)

  let ignore (r : Parser<'T, 'E>) = map (ignore) r


let noCache = () :> obj
let private noCacheId = hash noCache

type Parse(cacheKey : obj) =
  let id = hash (cacheKey)

  // Delay wraps the assembled Computation Expression in a function
  // This inject caching and a bottom up parse step for left-recursive combinators
  member this.Delay(f : ParserCombinator<Unit, 'T, 'E>) =
    let rec updateCache (state : State<Unit>) (result : ParseResult<'T, 'E>) =
      let e = state.View.End

      match
        result
        |> Parser.setStart state.View.Start
      with
      | Success s ->
        Success
          { s with
              Cache =
                s.Cache
                |> Map.add (id, e) (Complete(s.View, s.Value :> obj))
          }
      | Error s ->
        Error
          { s with
              Cache =
                s.Cache
                |> Map.add (id, e) (Fail(s.View, s.Value :> obj))
          }
      | Partial(fid, s) ->
        Partial(
          fid,
          { s with
              Cache =
                s.Cache
                |> Map.add (id, e) (Complete(s.View, s.Value :> obj))
          }
        )
      | Recursion(x, y) -> Recursion(x, y)

    let updatePath (state : State<Unit>) =
      { state with
          Path =
            if
              state.Path
              |> List.tryHead
              |> Option.contains id
            then
              id :: state.Path
            else
              state.Path
      }


    let delay (state : State<Unit>) (str : string) =
      let state = updatePath state
      let e = state.View.End

      match state.Cache |> Map.tryFind (id, e) with
      | Some(Fail(view, value)) ->
        Error
          {
            Value = value :?> 'E
            View = view
            Cache = state.Cache
            Path = state.Path
          }
      | Some(Recurse id) ->
        // Recursion detected, don't continue computation
        Recursion(set[id], state)
      | Some(Complete(v, value)) ->
        Success
          {
            Value = value :?> 'T
            View = v
            Cache = state.Cache
            Path = state.Path
          }
      | None ->
        let rec shiftReduce next prev c =
          match
            Parser.runParser (f ()) next str
            |> updateCache state
          with
          | Success s -> Success s
          | Error e -> prev |> Option.defaultValue (Error e)
          | Recursion(fid, state) -> Recursion(Set.add id fid, state)
          | Partial(fid, state) when not (Set.contains id fid) ->
            // Bubble Up - if there is no parent that can perform a shift-reduce, then we are done
            if
              state.Path
              |> List.tryFind (fun x -> fid |> Set.contains x)
              |> Option.isSome
            then
              Partial(fid, state)
            else
              Success state
          | Partial(_, result) ->
            let r = Success result

            shiftReduce { Parser.getState r with View = state.View } (Some r) (c + 1)

        let next = { state with Cache = Map.add (id, e) (Recurse id) state.Cache }

        shiftReduce next None 1

    if id <> noCacheId then
      delay
    else
      fun state str ->
        Parser.runParser (f ()) state str
        |> Parser.setStart state.View.Start

  member this.While(cond : Unit -> bool, p : Parser<'T, 'E>) = Parser.While cond p
  member this.Return(value : 'T) = Parser.Return value
  member this.ReturnFrom(p : Parser<'T, 'E>) = p
  member this.Bind(p : Parser<'T, 'E>, pc : ParserCombinator<'T, 'U, 'E>) : Parser<'U, 'E> = Parser.Bind p pc

// TODO: return/apply Rule instead of operating on strings directly
let parseString (x : string) =
  let f (s : State<Unit>) (str : string) =
    let fail = (str.Length - s.View.End) < x.Length

    if
      not fail
      && str.Substring(s.View.End, x.Length) = x
    then
      Success
        {
          Value = x
          Cache = s.Cache
          Path = s.Path
          View =
            {
              Start = s.View.End
              End = s.View.End + x.Length
            }
        }
    else
      Error
        {
          Value = ()
          Cache = s.Cache
          Path = s.Path
          View =
            {
              Start = s.View.Start
              End = s.View.Start
            }
        }

  f

let skipString (x : string) = parseString x |> Parser.ignore


let private isRecursion (k, v) =
  match v with
  | Recurse _ -> true
  | _ -> false

let private dropNewRecursions old k v =
  match v with
  | Recurse _ -> Set.contains k old
  | _ -> true


let rec runAll (xs : seq<Parser<'T, 'E>>) (state : State<Unit>) (str : string) =
  seq {
    match xs |> Seq.tryHead with
    | None -> ()
    | Some p ->
      let result = p state str
      yield result

      match result with
      | Success next -> yield! runAll (Seq.tail xs) (Parser.strip next) str
      | Partial(ids, next) ->
        yield!
          runAll (Seq.tail xs) (Parser.strip next) str
          |> Seq.map (Parser.makePartial ids)
      | _ -> ()
  }

// parse all in sequence and fail if one of the parsers fail
let parseAll (xs : seq<Parser<'T, 'E>>) (state : State<Unit>) (str : string) =
  runAll xs state str
  |> Seq.fold
    (fun acc result ->
      match acc, result with
      | Error e, _ -> Error e
      | Recursion(a, b), _ -> Recursion(a, b)
      | Success acc, Success next ->
        Success
          {
            Value = acc.Value @ [ next.Value ]
            Cache = next.Cache
            View = next.View
            Path = state.Path
          }
      | Partial(ids, acc), Success next ->
        Partial(
          ids,
          {
            Value = acc.Value @ [ next.Value ]
            Cache = next.Cache
            View = next.View
            Path = state.Path
          }
        )
      | Success acc, Partial(ids, next) ->
        Partial(
          ids,
          {
            Value = acc.Value @ [ next.Value ]
            Cache = next.Cache
            View = next.View
            Path = state.Path
          }
        )
      | Partial(ids, acc), Partial(ids2, next) ->
        Partial(
          (Set.union ids ids2),
          {
            Value = acc.Value @ [ next.Value ]
            Cache = next.Cache
            View = next.View
            Path = state.Path
          }
        )
      | _, Recursion(a, b) -> Recursion(a, b)
      | _, Error e -> Error e)
    (Success
      {
        Value = []
        Cache = state.Cache
        View = state.View
        Path = state.Path
      })

// returns longest sequence of successful parses
let parseLongest (xs : seq<Parser<'T, 'E>>) (state : State<Unit>) (str : string) =
  runAll xs state str
  |> Seq.fold
    (fun acc result ->
      match acc, result with
      | Error e, _ -> Error e
      | Recursion(a, b), _ -> Recursion(a, b)
      | acc, Recursion _ -> acc
      | acc, Error _ -> acc
      | Success acc, Success next ->
        Success
          {
            Value = acc.Value @ [ next.Value ]
            Cache = next.Cache
            View = next.View
            Path = state.Path
          }
      | Partial(ids, acc), Success next ->
        Partial(
          ids,
          {
            Value = acc.Value @ [ next.Value ]
            Cache = next.Cache
            View = next.View
            Path = state.Path
          }
        )
      | Success acc, Partial(ids, next) ->
        Partial(
          ids,
          {
            Value = acc.Value @ [ next.Value ]
            Cache = next.Cache
            View = next.View
            Path = state.Path
          }
        )
      | Partial(ids, acc), Partial(ids2, next) ->
        Partial(
          (Set.union ids ids2),
          {
            Value = acc.Value @ [ next.Value ]
            Cache = next.Cache
            View = next.View
            Path = state.Path
          }
        ))
    (Success
      {
        Value = []
        Cache = state.Cache
        View = state.View
        Path = state.Path
      })

// find best possible alternative
let rec bestOf (xs : List<Parser<'T, 'E>>) (state : State<Unit>) (str : string) : ParseResult<'T, 'E> =
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
    |> Seq.scan
      (fun (_, s) (p : Parser<'T, 'E>) ->
        let next = { s with View = state.View } // reset the cursor

        let next =
          Parser.parseAt p next str
          |> removeRecursionNodesOfAlternatives

        (Some next, Parser.getState next))
      (None, state)
    |> Seq.choose fst
    |> Seq.sortByDescending (fun x -> (Parser.getState x).View.End)
    |> Seq.toList

  let recursive =
    ys
    |> Seq.filter (function
      | Recursion _ -> true
      | _ -> false)

  let partials =
    ys
    |> Seq.filter (function
      | Partial _ -> true
      | _ -> false)

  // list of recursive parsers
  // if recursion is detected then we only have a partial result
  let ids =
    (Seq.concat [ recursive; partials ])
    |> Seq.choose (function
      | Recursion(ids, _) -> Some(ids)
      | Partial(ids, _) -> Some ids
      | _ -> None)
    |> Set.unionMany

  let complete =
    ys
    |> List.choose (function
      | Success x ->
        Some(
          if ids |> Set.count > 0 then
            Partial(ids, x)
          else
            Success x
        )
      | _ -> None)

  let errors =
    ys
    |> List.filter (function
      | Error _ -> true
      | _ -> false)

  Seq.concat [ complete; errors ]
  |> Seq.sortByDescending Parser.score
  |> Seq.head



// Return first and longest match
let pstrings (xs : Set<string>) (state : State<Unit>) (str : string) : ParseResult<string, Unit> =
  let str = str.Substring(state.View.End)

  let result =
    xs
    |> Seq.sortByDescending (fun x -> x.Length)
    |> Seq.tryFind (str.StartsWith)

  match result with
  | None ->
    Error
      {
        Value = ()
        View = state.View
        Path = state.Path
        Cache = state.Cache
      }
  | Some x ->
    Success
      {
        Value = x
        View =
          {
            Start = state.View.End
            End = state.View.End + x.Length
          }
        Path = state.Path
        Cache = state.Cache
      }

let pchars (xs : Set<char>) (state : State<Unit>) (str : string) : ParseResult<string, Unit> =
  let str = str.Substring(state.View.End)
  let result = xs |> Seq.tryFind (str.StartsWith)

  match result with
  | None ->
    Error
      {
        Value = ()
        View = state.View
        Path = state.Path
        Cache = state.Cache
      }
  | Some x ->
    Success
      {
        Value = (string x)
        View =
          {
            Start = state.View.End
            End = state.View.End + 1
          }
        Path = state.Path
        Cache = state.Cache
      }

let pcharRange (a : char) (b : char) (state : State<Unit>) (str : string) : ParseResult<string, Unit> =
  let c = str.[state.View.End]

  match c <= b && c >= a with
  | false ->
    Error
      {
        Value = ()
        View = state.View
        Path = state.Path
        Cache = state.Cache
      }
  | true ->
    Success
      {
        Value = (string c)
        View =
          {
            Start = state.View.End
            End = state.View.End + 1
          }
        Path = state.Path
        Cache = state.Cache
      }

let ptake (n : int) (state : State<Unit>) (str : string) : ParseResult<string, Unit> =
  Success
    {
      Value = str.Substring(state.View.End, n)
      View =
        {
          Start = state.View.End
          End = state.View.End + n
        }
      Cache = state.Cache
      Path = state.Path
    }

let lookBack (n : int) (state : State<Unit>) (str : string) : ParseResult<string, Unit> =
  if state.View.End >= n then
    Success
      {
        Value = str.Substring(state.View.End - n, n)
        View = state.View
        Cache = state.Cache
        Path = state.Path
      }
  else
    Error state

let takeWhile
  (f : 'S -> char -> 'S * bool)
  (initial : 'S)
  (state : State<Unit>)
  (str : string)
  : ParseResult<string, Unit> =
  let n =
    str.Substring(state.View.End)
    |> seq
    |> Seq.scan (fst >> f) (initial, true)
    |> Seq.takeWhile snd
    |> Seq.length
    |> fun x -> x - 1


  Success
    {
      Value = str.Substring(state.View.End, n)
      View =
        {
          Start = state.View.End
          End = state.View.End + n
        }
      Cache = state.Cache
      Path = state.Path
    }


let tryParse (p : Parser<'T, 'E>) (state : State<Unit>) (str : string) : ParseResult<Option<'T>, 'E> =
  match p state str with
  | Success x ->
    Success
      {
        Value = Some x.Value
        View = x.View
        Cache = x.Cache
        Path = x.Path
      }
  | Partial(ids, x) ->
    Partial(
      ids,
      {
        Value = Some x.Value
        View = x.View
        Cache = x.Cache
        Path = x.Path
      }
    )
  | Recursion(a, b) -> Recursion(a, b)
  | Error e ->
    Success
      {
        Value = None
        View = state.View
        Cache = e.Cache
        Path = e.Path
      }

module Seq =
  let rec cycle xs =
    seq {
      yield! xs
      yield! cycle xs
    }

// repeats provided parser up to max times and fails if it had less than min repetitions
// if max is null it will perform infinite repetitions
let repeat (min : int) (max : int) (p : Parser<'T, 'E>) (state : State<Unit>) (str : string) =
  let xs = Seq.cycle (seq { yield p })

  let results =
    parseLongest
      (if max > 0 then
         xs |> Seq.take max
       else
         xs)
      state
      str

  match results with
  | Success x ->
    if x.Value |> Seq.length >= min then
      Success x
    else
      Error(Parser.strip x)
  | Partial(ids, x) ->
    if x.Value |> Seq.length >= min then
      Partial(ids, x)
    else
      Error(Parser.strip x)
  | Recursion(a, b) -> Recursion(a, b)
  | Error x -> Error x

let recursive (f : (Ref<Parser<'T, 'E>>) -> Parser<'T, 'E>) : Parser<'T, 'E> =
  let mutable self : Ref<Parser<'T, 'E>> = ref (Unchecked.defaultof<Parser<'T, 'E>>)
  self := f (self)
  !self
