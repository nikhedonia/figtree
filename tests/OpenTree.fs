module Tests.OpenTree

open Xunit
open Figtree.OpenTree

type Expr =
| Plus of Expr * Expr
| Mult of Expr * Expr
| Id of string

let rec render (x: Expr) =
  match x with
  | Id str -> str
  | Plus (x, y) -> "(" + render x + " + " + render y + ")" 
  | Mult (x, y) -> "(" + render x + " * " + render y + ")"


type ExprTreeTests() =

    [<Fact>]
    let ```Should maintain operator precedence`` () =

        let seedX = seed (Id "x") // x
        let mulY = right 20 Mult (Id "y") // ... * y
        let addZ = right 10 Plus (Id "z") // ... + z

        let r = 
          seedX             // x
          |> extend addZ    // + z
          |> extend mulY    //    * y
          |> extend addZ    //        + z 
          |> extend mulY    //            * y
          |> extend addZ    //                + z
          |> seal

        Assert.Equal ("(((x + (z * y)) + (z * y)) + z)", render r)