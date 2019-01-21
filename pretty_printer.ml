module PrettyPrinter : sig
  include Pprinter.PPRINTER
  val set_width: int -> unit
  val set_indent: int -> unit
end = struct
  open Nix.Ast
  open PPrintEngine
  open PPrintCombinators

  let out_width = ref 80
  let set_width i = out_width := i

  let indent = ref 2
  let set_indent i = indent := i

  let rec doc_of_expr = function
    | BinaryOp(op, lhs, rhs) ->
      let lvl = prec_of_bop op in
      let lhs_doc = maybe_parens lvl lhs in
      let rhs_doc = maybe_parens lvl rhs in
      (* TODO: don't use parens if it's a chain of ops, e.g. 1+1+1 *)
      infix !indent 1 (doc_of_bop op) lhs_doc rhs_doc

    | UnaryOp(op, e) ->
      precede (doc_of_uop op) (maybe_parens (prec_of_uop op) e)

    | Cond(e1, e2, e3) ->
      surround !indent 1
        (soft_surround !indent 1
           (string "if") (doc_of_expr e1) (string "then"))
        (doc_of_expr e2)
        (string "else" ^^
         (nest !indent (break 1 ^^ doc_of_expr e3)))

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
        maybe_parens 4 e ^^ string "?" ^^
        group (break 1 ^^ separate_map dot doc_of_expr path)

    | Let(bs, e) ->
      surround !indent 1
        (string "let")
        (separate_map (break 1) doc_of_binding bs)
        (prefix !indent 1 (string "in") (doc_of_expr e))

    | Val v ->
      doc_of_val v

    | Id id ->
      string id

    | Select(e, path, oe) ->
      maybe_parens 1 e ^^ dot ^^
      doc_of_attpath path ^^
      optional (fun e ->
          space ^^ string "or" ^^
          nest !indent ( break 1 ^^ maybe_parens 1 e)
        ) oe

    | Apply(e1, e2) ->
      prefix !indent 1 (maybe_parens 2 e1) (maybe_parens 2 e2)

    | Aquote e ->
      surround !indent 0 (string "${") (doc_of_expr e) (string "}")

  and maybe_parens lvl e =
    if prec_of_expr e >= lvl then
      surround !indent 0 lparen (doc_of_expr e) rparen
    else
      doc_of_expr e

  and doc_of_attpath path =
    separate_map dot doc_of_expr path

  and doc_of_paramset(params, ellipsis) =
    let ps = List.map doc_of_param params @ match ellipsis with
      | Some _ -> [string "..."]
      | None -> []
    in
    surround !indent 0
      lbrace
      (separate (comma ^^ break 1) ps)
      rbrace

  and doc_of_param(id, oe) =
    string id ^^ optional (fun e ->
        qmark ^^ space ^^ doc_of_expr e
      ) oe

  and doc_of_binding = function
    | AttrPath(path, e) ->
      (doc_of_attpath path) ^^
      space ^^ equals ^^ space ^^
      doc_of_expr e ^^ semi

    | Inherit(oe, ids) ->
      let id_docs = List.map string ids in
      let xs = flow (break 1) (
          match oe with
          | Some e -> (parens (doc_of_expr e)) :: id_docs
          | None -> id_docs
        )
      in
      soft_surround !indent 0 (string "inherit" ^^ space) xs semi

  and doc_of_bop = function
    | Plus -> plus
    | Minus -> minus
    | Mult -> star
    | Div -> slash
    | Gt -> rangle
    | Lt -> langle
    | Lte -> string "<="
    | Gte -> string ">="
    | Eq -> string "=="
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
            surround !indent 0
              (string "${")
              (doc_of_expr e)
              (string "}" ^^ string s)
          ) xs ))

    | IStr(i, start, xs) ->
      let qq = string "''" in
      let skip_first_line xs =
        match xs with
        | ws :: rest ->
          if String.trim ws |> String.equal "" then rest else xs
        | _ -> xs
      in
      let str skip s =
        String.split_on_char '\n' s
        |> (match skip with | `Start -> skip_first_line | `Mid -> (fun x -> x))
        |> List.map (fun s ->
            let len = String.length s in
            let s' = if len >= i then String.sub s i (len - i) else s in
            string s'
          )
        |> separate hardline
      in
      enclose (qq ^^ hardline) qq (
        str `Start start ^^
        concat (List.map (fun (e, s) ->
            enclose (string "${") rbrace (doc_of_expr e) ^^
            str `Mid s
          ) xs ))

    | Int x | Float x | Path x | SPath x | HPath x | Uri x | Bool x ->
      string x

    | Lambda(pattern, body) ->
      let pat = match pattern with
        | Alias id ->
          string id
        | ParamSet ps ->
          doc_of_paramset ps
        | AliasedSet(id, ps) ->
          doc_of_paramset ps ^^
          group (break 1 ^^ at ^^ break 1 ^^ string id)
      in
      flow (break 1) [
        pat ^^ colon;
        doc_of_expr body
      ]

    | List es ->
      surround !indent 1
        lbracket
        (separate_map (break 1) doc_of_expr es)
        rbracket

    | AttSet bs ->
      surround !indent 1
        lbrace
        (group (separate_map (break 1) doc_of_binding bs))
        rbrace

    | RecAttSet bs ->
      string "rec" ^^ space ^^ surround !indent 1
        lbrace
        (group (separate_map (break 1) doc_of_binding bs))
        rbrace

    | Null ->
      string "null"

  let print chan expr =
    ToChannel.pretty 0.7 !out_width chan (doc_of_expr expr)

end

include PrettyPrinter
