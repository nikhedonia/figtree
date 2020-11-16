module Tests.Combinator

open Xunit
open Figtree.Combinator


type Expr = 
| Op of string * Expr * Expr
| Nested of Expr
| Identifier of string 

type InfixOperator<'T> =
  {
    Name: string
    Left: int
    Right: int
    Args: List<'T>
  }

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


// type RepetitiveExprTests() =

//   let parseX: Parser<Expr, Unit> = Parse "x" {
//     let! x = parseString "x"
//     return Identifier x
//   }

//   let nested (expr: Parser<Expr, Unit>): Parser<Expr, Unit> = Parse ("nested", expr) {
//     do! skipString "("
//     let! x = expr
//     do! skipString ")"
//     return Nested x
//   }

//   let createExprTree (xs: List<Token*Expr>): Expr = 

//     let precedenceTable = Map.ofSeq [
//       ("", 0)
//       ("+", 10)
//       ("-", 10)
//       ("*", 20)
//       ("/", 20)
//     ] 

//     match xs with
//     | (Token op1, ex)::(Token op2, ey)::tail -> ()

//   let binary (expr: Parser<Expr, Unit>): Parser<Expr, Unit> = Parse ("binary", expr) {
//     let! x =
//       bestOf [
//         parseX
//         nested expr
//       ]

//     let! ys = 
//       (Parse () {
//         let! token = bestOf [
//           parseString "+" |>  Parser.map Token
//           parseString "*" |>  Parser.map Token
//         ]

//         let! node = bestOf [
//           parseX
//           nested expr
//         ]
//         return  (token, node)
//       }) 
//       |> repeat 1 0


//     return createExprTree ((Token "", x)::ys)
//   }
    
//   let rec expr = Parse "expr" {
//     return! bestOf [
//       binary expr 
//       nested expr
//       parseX
//     ]
//   }
 
//   [<Fact>]
//   let ``can parse simple expr`` () =
//     let str = "x+x"
//     match Parser.parse expr str with
//     | Success x -> 
//       Assert.Equal(str.Length, x.View.End)
//       ()
//     | r -> 
//       System.Console.WriteLine "ohh No"
//       System.Console.WriteLine (string r)
//       Assert.True(false)

//   [<Fact>]
//   let ``can parse simple expr with parents`` () =
//     let str = "(x+x)"
//     match Parser.parse expr str with
//     | Success x -> 
//       Assert.Equal(str.Length, x.View.End)
//     | r -> 
//       System.Console.WriteLine "ohh No"
//       System.Console.WriteLine (string r)
//       Assert.True(false)

//   [<Fact>]
//   let ``can parse complex expr`` () =
//     let str = "(((((x)))+(x+x)))"
//     match Parser.parse expr str with
//     | Success x -> 
//       Assert.Equal(str.Length, x.View.End)
//     | r -> 
//       System.Console.WriteLine "ohh No"
//       System.Console.WriteLine (string r)
//       Assert.True(false)