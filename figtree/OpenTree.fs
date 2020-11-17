module Figtree.OpenTree

// How to introduce operator precedence in parser combinators?
// without lookahead and without precedence parsers?!
//       Or how to defer parse-tree disambiguation without lookahead


// How do you transition from a tree generated by a partial parse "x + y" to eg. "x + y * z"
// Do the trees show any similarities? 

// Examples:

// A:
// x + y + z
//      +
//   +     z
// x   y

// B:
// x + y * z
//     +
//  x     * 
//      y    z

// C: Operator of higher / equal => extend tree "down"
// x + y * z * w
//     +
//  x     * 
//      y   (*) 
//         z   w

// D: Operator of lower precedence =>  extend tree to the "top"
// x + y * z + w
//            (+)
//     +              w
//  x      * 
//      y      z     


type TreeAction<'Expr> =
| Down of ('Expr->'Expr)
| Top of ('Expr->'Expr)

// Intermediate Datastructure for fast Tree Extension
type OpenTree<'Expr> = int * (TreeAction<'Expr> -> 'Expr)
type TreeExtension<'Expr> = int * (('Expr*'Expr->'Expr) * 'Expr)

let seed (expr: 'Expr): OpenTree<'Expr> =
  0, fun (action: TreeAction<'Expr>) ->
  match action with
  | Down extend -> extend expr
  | Top extend -> extend expr

// creates a tree extension consisting of the right hand side (operator * Expr) of a binary operation - eg. ? * x
let right (bp: int) (op: 'Expr*'Expr->'Expr) (rhs: 'Expr): TreeExtension<'Expr> =
  (bp, (op, rhs))

let private extendUp (x: TreeAction<'Expr> -> 'Expr) ( (op, rhs): ('Expr*'Expr->'Expr) * 'Expr ) (action: TreeAction<'Expr>) =
  match action with
  | Down extend -> x (Top (fun lhs -> op (lhs, extend rhs)))
  | Top extend -> extend (x (Top (fun lhs -> op (lhs, rhs))))
  
let private extendDown (x: TreeAction<'Expr> -> 'Expr) ( (op, rhs): ('Expr*'Expr->'Expr) * 'Expr) (action: TreeAction<'Expr>) =
  match action with
  | Down extend -> x (Down (fun lhs -> op (lhs, extend rhs)))
  | Top extend -> extend (x (Down (fun lhs -> op (lhs, rhs))))

// extend up if binding power of rhs higher otherwise extend down
let extend (b, right) (a, left): OpenTree<'Expr> =
  if a <= b
  then b, (extendDown left right)
  else b, (extendUp left right)

// closes the openTree from further extension
let seal (x: OpenTree<'Expr>): 'Expr = (snd x) (Top id)
