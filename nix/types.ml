type binary_op =
    | Plus
    | Minus
    | Mult
    | Div
    | Gt
    | Lt
    | Lte
    | Gte
    | Neq
    | Or
    | And
    | Ipml
    | Merge
    | Concat

type unary_op =
  | Negate
  | Not

type expr =
  | BinaryOp of binary_op * expr * expr
  | UnaryOp of unary_op * expr
  | Cond of expr * expr * expr
  | With of expr * expr
  | Assert of expr * expr
  | Let of (id * expr) list * expr
  | Val of value
  | Id of id
  | Select of expr * expr list
  | SelectDef of expr * expr list * value
  | Apply of expr * expr list


and value =
  (* Str is a string start, followed by arbitrary number of antiquotations and
     strings that separate them *)
  | Str of string * (expr * string) list
  (* IStr is an indented string, so it has the extra integer component which
     indicates the indentation *)
  | IStr of int * string * (expr * string) list
  | Int of string
  | Float of string
  | Path of string
  | SPath of string
  | HPath of string
  | Uri of string
  | Bool of string
  | Lambda of pattern * expr
  | List of expr list
  | AttSet of attr list
  | RecAttSet of attr list

and pattern =
  | Alias of id
  | ParamSet of param_set
  | AliasedSet of id * param_set

and param_set =
  | CompleteSet of param list
  | IncompleteSet of param list

and param =
  | Param of id
  | DefaultParam of id * value

and attr =
  | IdKey of id * value
  (* Nix allows an aribtrary double-quoted strings as attribute names, so the
     first value in the tuple should be a string.
     TODO: use GADT *)
  | StrKey of value * value
  | Inherit of id option * id list

and id = string
