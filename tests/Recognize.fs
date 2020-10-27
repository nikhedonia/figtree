namespace Tests

open Xunit
open Figtree.Rule
open Figtree.Match
open Figtree.Recognize
open Figtree.Produce

type MyTests () =
    let rules =
        ruleTable [
            "digit", CharRange ('0', '9')
            "number", Repeat(Ref"digit", 1, 0)
            "char", AnyOf([ CharRange ('a', 'z'); CharRange ('A', 'Z'); CharSet(set['_';'$'])])
            "charOrNum", AnyOf([Ref"char"; Ref"digit"])
            "identifier", EachOf [Ref"char"; Repeat(Ref"charOrNum",0,0)]
            "ws", Repeat(CharSet (set[' ']), 1, 0)
            "op", CharSet (set['-'; '+'])
            "expr", Ref"identifier"
            "expr", Ref"number"
            "expr", EachOf[Ref"expr"; Repeat(Ref"ws", 0, 1); Ref"op"; Repeat(Ref"ws", 0, 1); Ref"expr"]
        ]
        |> RuleTable.verify
        |> Result.get

    [<Fact>]
    let ``can handle simple recursion`` () =
        let str = "foo + bar + baz"
        let x =
            match recognize rules "expr" str 0 with
            | Some (Tree ((p, _)::xs, s, e, _)) -> (p, s, e)
            | None -> ("", 0, 0)

        Assert.Equal(x, ("expr", 0, str.Length))

    [<Fact>]
    let ``can produce`` () =
        let str = produce rules "expr"
        System.Console.WriteLine str
        Assert.Equal(1,1)