module Tests.Combinator

open Xunit
open Figtree.Combinator


type MyTests() =

  let expr = recursive(fun expr ->
    let binary (op: string) = Parse ("binary", op) {
      let! x = !expr
      let! _ = parseString op
      let! y = !expr
      return x + y
    }

    let nested = Parse "nested" {
      let! _ = parseString "(" 
      let! x = !expr
      let! _ = parseString ")"
      return x
    }

    let x = Parse "x" {
      let! _ = parseString "x"
      return 1
    }
    
    Parse "expr" {
      let! r = bestOf [
        binary "+"
        binary "-"
        nested
        x
      ]
      return 1
    }
  )

  [<Fact>]
  let ``can parse brackets`` () =

    let str = "((x))"
    match Parser.parse expr str with
    | Success x -> 
      Assert.Equal(x.Value, 1)
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
