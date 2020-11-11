namespace Tests

open Xunit
open Figtree.Rule

type VerifyTests () =

  [<Fact>]
  let ``verify finds simple case`` () =
    let actual =
      ruleTable [
        "a", Ref "b"
        "b", Ref "c"
        "x", Ref "y"
      ]
      |> RuleTable.verify

    let expected =
      Error
        [
          UndefinedReference ("b", "c")
          UndefinedReference ("x", "y")
        ]

    Assert.Equal(actual, expected)

    ()
