module Integration.Json

open Xunit
open Figtree.Combinator

type Json =
  | JFloat of float
  | JInt of int
  | JString of string
  | JList of List<Json>
  | JObj of Map<string, Json>

type StringCollector =
  | Escaping
  | Searching


let skipWS =
  pchars (set " \n\t\r")
  |> repeat 0 0
  |> Parser.ignore


let parseJNumber =
  Parse("number") {
    do! skipWS

    let! digits =
      pchars (set "0123456789")
      |> repeat 1 0
      |> Parser.map (String.concat "")

    let! hasDot = tryParse (pchars (set "."))

    return!
      match hasDot with
      | Some x ->
        Parse() {
          let! moreDigits =
            pchars (set "0123456789")
            |> repeat 1 0
            |> Parser.map (String.concat "")

          return JFloat(float (digits + x + moreDigits))
        }
      | None -> Parse() { return JInt(int digits) }
  }

let private parseJStr =
  Parse("parseJsString") {
    do! skipWS
    do! skipString "\""

    let! str =
      takeWhile
        (fun s c ->
          match s, c with
          | Escaping, '\\' -> Escaping, true
          | Escaping, _ -> Searching, true
          | Searching, '"' -> Searching, false
          | Searching, _ -> Searching, true)
        Searching

    do! skipString "\""
    return str
  }

let parseJString = parseJStr |> Parser.map JString

let parseJList (json : Parser<Json, Unit>) =
  Parse(json, "list") {
    do! skipWS
    do! skipString "["
    let! item = tryParse json

    let! items =
      match item with
      | Some item ->
        Parse() {
          let! items =
            Parse() {
              do! skipWS
              do! skipString ","
              do! skipWS
              return! json
            }
            |> repeat 0 0

          return List.concat [ [ item ]; items ]
        }
      | None -> Parse() { return [] }

    do! skipWS
    do! parseString "]" |> Parser.ignore
    return JList items
  }

let parseJKeyValuePair (json : Parser<Json, Unit>) =
  Parse(json, "kvp") {
    do! skipWS
    let! key = parseJStr
    do! skipWS
    do! skipString ":"
    let! value = json
    return key, value
  }

let parseJObj (json : Parser<Json, Unit>) =
  Parse(json, "obj") {
    do! skipWS
    do! skipString "{"
    let! item = tryParse (parseJKeyValuePair json)

    let! pairs =
      match item with
      | Some item ->
        Parse() {
          let! items =
            Parse() {
              do! skipWS
              do! skipString ","
              do! skipWS
              return! parseJKeyValuePair json
            }
            |> repeat 0 0

          return List.concat [ [ item ]; items ]
        }
      | None -> Parse() { return [] }

    do! skipWS
    do! parseString "}" |> Parser.ignore
    return JObj(Map.ofSeq pairs)
  }

let parseJson =
  recursive (fun self ->
    Parse "Json" {
      let parseJson = self.Value

      return! bestOf [ parseJNumber; parseJString; parseJList parseJson; parseJObj parseJson ]
    })

type JSonTests() =
  [<Fact>]
  let ``can parse list with whitespace`` () =
    let str = "[1 , [], [1], [ 1 ], [2 , 3 ]  ,  4]"

    match Parser.parse parseJson str with
    | Success x ->
      Assert.Equal(str.Length, x.View.End)

      Assert.Equal(
        JList
          [
            JInt 1
            JList []
            JList [ JInt 1 ]
            JList [ JInt 1 ]
            JList [ JInt 2; JInt 3 ]
            JInt 4
          ],
        x.Value
      )
    | r ->
      System.Console.WriteLine "ohh No"
      System.Console.WriteLine(string r)
      Assert.True(false)

  [<Fact>]
  let ``can parse simple obj`` () =
    let str = "{ \"a\" : 1234, \"b\" : {}, \"c\": [] }"

    match Parser.parse parseJson str with
    | Success x ->
      Assert.Equal(str.Length, x.View.End)

      Assert.Equal(
        JObj
        <| Map.ofList [ ("a", JInt 1234); ("b", JObj Map.empty); ("c", JList []) ],
        x.Value
      )
    | r ->
      System.Console.WriteLine "ohh No"
      System.Console.WriteLine(string r)
      Assert.True(false)
