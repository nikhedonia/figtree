module Tests.Combinator

open Xunit
open Figtree.Combinator


type MyTests() =

    let mutable exprR = Parse "dummy" { return 0; } 

    let binary (op: string) = Parse ("binary", op) {
        let! x = exprR
        let! _ = parseString op
        let! y = exprR
        return x + y
    }

    let nested = Parse "nested" {
        let! _ = parseString "("
        let! x = exprR
        let! _ = parseString ")"
        return x
    }

    let x = Parse "x" {
        let! _ = parseString "x"
        return 1
    }
    
    let expr = Parse "expr" {
        let! r = bestOf [
            binary "+"
            binary "-"
            nested
            x
        ]
        return 1
    }




    [<Fact>]
    let ``can parse brackets`` () =
        exprR <- expr
        
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
        exprR <- expr
        
        let str = "x+x"
        match Parser.parse expr str with
        | Success x -> 
            //Assert.Equal(x.Value, 1)
            Assert.Equal(str.Length, x.View.End)
            ()
        | r -> 
            System.Console.WriteLine "ohh No"
            System.Console.WriteLine (string r)
            Assert.True(false)

    [<Fact>]
    let ``can parse simple expr with parents`` () =
        exprR <- expr
        
        let str = "(x+x)"
        match Parser.parse expr str with
        | Success x -> 
            Assert.Equal(str.Length, x.View.End)
            ()
        | r -> 
            System.Console.WriteLine "ohh No"
            System.Console.WriteLine (string r)
            Assert.True(false)

    [<Fact>]
    let ``can parse complex expr`` () =
        exprR <- expr
        
        let str = "(((((x)))+(x-x)))"
        match Parser.parse expr str with
        | Success x -> 
            //Assert.Equal(x.Value, 1)
            Assert.Equal(str.Length, x.View.End)
            ()
        | r -> 
            System.Console.WriteLine "ohh No"
            System.Console.WriteLine (string r)
            Assert.True(false)
