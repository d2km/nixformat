type op =
    | Plus
    | Minus
    | Mult
    | Div

type expr =
  | Op of op*expr*expr
  | Val of string
and assoc_pair =
  id * expr
and let_expr =
  assoc_pair list * expr
and lambda =
  pattern * expr
and pattern =
  | Id of id
  | Assoc of assoc_pair
and id = string
