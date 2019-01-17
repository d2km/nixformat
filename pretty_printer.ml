module PrettyPrinter : sig
  include Pprinter.PPRINTER
  val set_width: int -> unit
  val set_indent: int -> unit
end  = struct
  open Nix.Ast
  open PPrintEngine
  open PPrintCombinators

  let out_width = ref 80
  let set_width i = out_width := i

  let indent = ref 2
  let set_indent i = indent := i

  let rec doc_of_expr = function
    | BinaryOp(op, lhs, rhs) ->
      infix !indent 1 (doc_of_bop op) (doc_of_expr lhs) (doc_of_expr rhs)

    | UnaryOp(op, e) ->
      precede (doc_of_uop op) (doc_of_expr e)

    | Cond(e1, e2, e3) ->
      flow (break 1) [
        string "if";
        doc_of_expr e1 ^^ break 1 ^^ string "then";
        nest !indent (doc_of_expr e2);
        string "else";
        nest !indent (doc_of_expr e3)
      ]

    | With(e1, e2) ->
      flow (break 1) [
        string "with";
        doc_of_expr e1 ^^ semi;
        doc_of_expr e2
      ]

    | Assert(e1, e2) ->
      flow (break 1) [
        string "assert";
        doc_of_expr e1 ^^ semi;
        doc_of_expr e2
      ]

    | Test(e, path) ->
        doc_of_expr e ^^ string "?" ^^
        group (break 1 ^^ separate_map dot doc_of_expr path)

    | Let(bs, e) ->
      flow (break 1) [
        prefix !indent 1
          (string "let")
          (flow (break 1) (List.map doc_of_binding bs));
        prefix !indent 1 (string "in") (doc_of_expr e)
      ]

    | Val v ->
      doc_of_val v

    | Id id ->
      string id

    | Select(e, path, oe) ->
      maybe_parens e ^^
      dot ^^
      separate_map dot doc_of_expr path ^^
      optional (fun e ->
          flow (break 1) [ string "or"; doc_of_expr e]
        ) oe

    | Apply(e1, e2) ->
      maybe_parens e1 ^^ break 1 ^^ maybe_parens e2

    | Aquote e ->
      string "${" ^^ group (
        break 1 ^^
        nest !indent (group (doc_of_expr e)) ^^
        break 1
      ) ^^ string "}"

  and maybe_parens = function
    | e -> doc_of_expr e

  and doc_of_binding = function
    | AttrPath(es, e) ->
      flow (break 1) [
        separate_map dot doc_of_expr es;
        equals;
        doc_of_expr e ^^ semi
      ]
    | Inherit(oe, ids) ->
      flow (break 1) [
        optional (fun e -> parens (doc_of_expr e) ) oe;
        separate_map dot string ids
      ]

  and doc_of_bop = function
    | Plus -> plus
    | Minus -> minus
    | Mult -> star
    | Div -> slash
    | Gt -> rangle
    | Lt -> langle
    | Lte -> string "<="
    | Gte -> string ">="
    | Eq -> equals
    | Neq -> string "!="
    | Or -> string "||"
    | And -> string "&&"
    | Impl -> string "->"
    | Merge -> string "//"
    | Concat -> string "++"

  and doc_of_uop = function
    | Negate -> minus
    | Not -> bang

  and doc_of_val = function
    | Str(start, xs) ->
      dquotes (
        string start ^^
        concat (List.map (fun (e, s) ->
            string "${" ^^
            group (break 1 ^^ doc_of_expr e) ^^
            break 1 ^^ string "}" ^^ string s
          ) xs ))

    | IStr(_, _, _) ->
      string "<indented>"

    | Int x | Float x | Path x | SPath x | HPath x | Uri x | Bool x ->
      string x

    | Lambda(_, _) ->
      string "<lambda>"

    | List es ->
      brackets (separate_map (break 1) doc_of_expr es)

    | AttSet bs ->
      lbrace ^^ nest !indent (
        flow (break 1) (List.map doc_of_binding bs);
      ) ^^ rbrace

    | RecAttSet bs ->
      string "rec" ^^ space ^^ lbrace ^^ nest !indent (
        flow (break 1) (List.map doc_of_binding bs);
      ) ^^ rbrace

    | Null ->
      string "null"

  let print chan = function
    | expr ->
      ToChannel.pretty 0.7 !out_width chan (doc_of_expr expr)
end

include PrettyPrinter
