module Tests.Combinator

open Xunit
open Figtree.Combinator
open Figtree.OpenTree


type Expr = 
| Op of string * Expr * Expr
| Nested of Expr
| Identifier of string 

let rec render (x: Expr) =
  match x with
  | Identifier str -> str
  | Op (op, x, y) -> "(" + render x + " " + op + " " + render y + ")" 

type RecursiveExprTests() =

  let expr = recursive(fun expr ->
    let binary (op: string) = Parse ("binary", op) {
      let! x = !expr
      let! o = parseString op
      let! y = !expr
      return Op (o, x, y)
    }

    let nested = Parse "nested" {
      let! _ = parseString "(" 
      let! x = !expr
      let! _ = parseString ")"
      return Nested x
    }

    let parseX = parseString "x" |> Parser.map Identifier
    
    Parse "expr" {
      return! bestOf [
        binary "+"
        binary "-"
        nested
        parseX
      ]
    }
  )

  [<Fact>]
  let ``can parse brackets`` () =

    let str = "((x))"
    match Parser.parse expr str with
    | Success x -> 
      Assert.Equal(str.Length, x.View.End)
      ()
    | r -> 
      System.Console.WriteLine "ohh No"
      System.Console.WriteLine (string r)
      Assert.True(false)

  [<Fact>]
  let ``can parse simple expr`` () =
      
    let str = "x+x"
    match Parser.parse expr str with
    | Success x -> 
      Assert.Equal(str.Length, x.View.End)
      ()
    | r -> 
      System.Console.WriteLine "ohh No"
      System.Console.WriteLine (string r)
      Assert.True(false)

  [<Fact>]
  let ``can parse simple expr with parents`` () =
    let str = "(x+x)"
    match Parser.parse expr str with
    | Success x -> 
      Assert.Equal(str.Length, x.View.End)
    | r -> 
      System.Console.WriteLine "ohh No"
      System.Console.WriteLine (string r)
      Assert.True(false)

  [<Fact>]
  let ``can parse complex expr`` () =
    let str = "(((((x)))+(x-x)))"
    match Parser.parse expr str with
    | Success x -> 
      Assert.Equal(str.Length, x.View.End)
    | r -> 
      System.Console.WriteLine "ohh No"
      System.Console.WriteLine (string r)
      Assert.True(false)


type RepetitiveExprTests() =

  let parseX: Parser<Expr, Unit> = Parse "x" {
    let! x = parseString "x"
    return Identifier x
  }

  let nested (expr: Parser<Expr, Unit>): Parser<Expr, Unit> = Parse ("nested", expr) {
    do! skipString "("
    let! x = expr
    do! skipString ")"
    return Nested x
  }


  let binary (expr: Parser<Expr, Unit>): Parser<Expr, Unit> = Parse ("binary", expr) {
    let! x =
      bestOf [
        parseX
        nested expr
      ]

    let! ys = 
      (Parse () {
        let! op = bestOf [
          parseString "+"
          parseString "*" 
        ]

        let! rhs = bestOf [
          parseX
          nested expr
        ]

        let bp =
          match op with
          | "*" -> 20
          | "/" -> 20
          | "+" -> 10
          | "-" -> 10
          | _ -> 0

        return right bp (fun (lhs, rhs) -> Op (op, lhs, rhs)) rhs 
      }) 
      |> repeat 1 0
    
    let root = seed x
    let exprTree = ys |> Seq.fold (fun tree branch -> tree |> extend branch) root |> seal
    return exprTree
  }
    
  let rec expr = Parse "expr" {
    return! bestOf [
      binary expr 
      nested expr
      parseX
    ]
  }
 
  [<Fact>]
  let ``can parse simple expr`` () =
    let str = "x"
    match Parser.parse expr str with
    | Success x -> 
      Assert.Equal(str.Length, x.View.End)
      ()
    | r -> 
      System.Console.WriteLine "ohh No"
      System.Console.WriteLine (string r)
      Assert.True(false)

 
  [<Fact>]
  let ``can parse simple expr`` () =
    let str = "x+x*x+x"
    match Parser.parse expr str with
    | Success x -> 
      Assert.Equal(str.Length, x.View.End)
      Assert.Equal(str.Length, x.View.End)
      ()
    | r -> 
      System.Console.WriteLine "ohh No"
      System.Console.WriteLine (string r)
      Assert.True(false)
