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

  let place_comments cs loc =
    let open Nix.Location in
    let open Nix.Comments in
    let rec take_while pred q =
      match try Some (Queue.peek q) with Queue.Empty -> None with
      | Some x ->
        if pred x then
          (ignore (Queue.pop q); x :: take_while pred q)
        else
          []
      | None -> []
    in
    let before = fun comment ->
      match compare comment.location loc with
      | Before -> true
      | _ -> false
    in
    let xs = take_while before cs in
    concat_map (fun c -> string (to_string c) ^^ hardline) xs


  let rec doc_of_expr cs = function
    | BinaryOp(op, lhs, rhs, loc) ->
      let lvl = prec_of_bop op in
      let comments = place_comments cs loc in
      comments ^^
      (
        let lhs_doc = maybe_parens cs lvl lhs in
        let op_doc = doc_of_bop op  in
        let rhs_doc = maybe_parens cs lvl rhs in
        infix !indent 1 op_doc lhs_doc rhs_doc
      )

    | UnaryOp(op, e, loc) ->
      let comments = (place_comments cs loc) in
      comments ^^
      precede (doc_of_uop op) (maybe_parens cs (prec_of_uop op) e)

    | Cond(e1, e2, e3, loc) ->
      let comments = (place_comments cs loc) in
      comments ^^
      surround !indent 1
        (soft_surround !indent 1
           (string "if") (doc_of_expr cs e1) (string "then"))
        (doc_of_expr cs e2)
        (string "else" ^^
         (nest !indent (break 1 ^^ doc_of_expr cs e3)))

    | With(e1, e2, loc) ->
      let comments = place_comments cs loc in
      comments ^^
      flow (break 1) [
        string "with";
        doc_of_expr cs e1 ^^ semi;
        doc_of_expr cs e2
      ]

    | Assert(e1, e2, loc) ->
      let comments = place_comments cs loc in
      comments ^^
      flow (break 1) [
        string "assert";
        doc_of_expr cs e1 ^^ semi;
        doc_of_expr cs e2
      ]

    | Test(e, path, loc) ->
      let comments = place_comments cs loc in
      comments ^^
      maybe_parens cs 4 e ^^ string "?" ^^
      group (break 1 ^^ separate_map dot (doc_of_expr cs) path)

    | Let(bs, e, loc) ->
      let comments = place_comments cs loc in
      comments ^^
      surround !indent 1
        (string "let")
        (separate_map (break 1) (doc_of_binding cs) bs)
        (prefix !indent 1 (string "in") (doc_of_expr cs e))

    | Val (v, loc) ->
      let comments = place_comments cs loc in
      comments ^^ doc_of_val cs v

    | Id (id, loc) ->
      let comments = place_comments cs loc in
      comments ^^ string id

    | Select(e, path, oe, loc) ->
      let comments = place_comments cs loc in
      comments ^^
      maybe_parens cs 1 e ^^ dot ^^
      doc_of_attpath cs path ^^
      optional (fun e ->
          space ^^ string "or" ^^
          nest !indent ( break 1 ^^ maybe_parens cs 1 e)
        ) oe

    | Apply(e1, e2, loc) ->
      let comments = place_comments cs loc in
      comments ^^
      prefix !indent 1 (maybe_parens cs 2 e1) (maybe_parens cs 2 e2)

    | Aquote (e, loc) ->
      let comments = place_comments cs loc in
      comments ^^
      surround !indent 0 (string "${") (doc_of_expr cs e) (string "}")

  and maybe_parens cs lvl e =
    if prec_of_expr e > lvl then
      surround !indent 0 lparen (doc_of_expr cs e) rparen
    else
      doc_of_expr cs e

  and doc_of_attpath cs path =
    separate_map dot (doc_of_expr cs) path

  and doc_of_paramset cs (params, ellipsis) =
    let ps = List.map (doc_of_param cs) params @ match ellipsis with
      | Some _ -> [string "..."]
      | None -> []
    in
    surround !indent 0
      lbrace
      (separate (comma ^^ break 1) ps)
      rbrace

  and doc_of_param cs (id, oe) =
    string id ^^ optional (fun e ->
        qmark ^^ space ^^ doc_of_expr cs e
      ) oe

  and doc_of_binding cs = function
    | AttrPath(path, e) ->
      (doc_of_attpath cs path) ^^
      space ^^ equals ^^ space ^^
      doc_of_expr cs e ^^ semi

    | Inherit(oe, ids) ->
      let id_docs = List.map string ids in
      let xs = flow (break 1) (
          match oe with
          | Some e -> (parens (doc_of_expr cs e)) :: id_docs
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

  and doc_of_val cs = function
    | Str(start, xs) ->
      dquotes (
        string start ^^
        concat (List.map (fun (e, s) ->
            surround !indent 0
              (string "${")
              (doc_of_expr cs e)
              (string "}" ^^ string s)
          ) xs ))

    | IStr(i, start, xs) ->
      let qq = string "''" in
      let str s =
        String.split_on_char '\n' s
        |> List.map (fun s ->
            let len = String.length s in
            let s' = if len >= i then String.sub s i (len - i) else s in
            string s'
          )
        |> separate hardline
      in
      enclose qq qq (
        str start ^^
        concat (List.map (fun (e, s) ->
            enclose (string "${") rbrace (doc_of_expr cs e) ^^ str s
          ) xs ))

    | Int x | Float x | Path x | SPath x | HPath x | Uri x | Bool x ->
      string x

    | Lambda(pattern, body) ->
      let pat = match pattern with
        | Alias id ->
          string id
        | ParamSet ps ->
          doc_of_paramset cs ps
        | AliasedSet(id, ps) ->
          doc_of_paramset cs ps ^^
          group (break 1 ^^ at ^^ break 1 ^^ string id)
      in
      flow (break 1) [
        pat ^^ colon;
        doc_of_expr cs body
      ]

    | List es ->
      surround !indent 1
        lbracket
        (separate_map (break 1) (doc_of_expr cs) es)
        rbracket

    | AttSet bs ->
      surround !indent 1
        lbrace
        (group (separate_map (break 1) (doc_of_binding cs) bs))
        rbrace

    | RecAttSet bs ->
      string "rec" ^^ space ^^ surround !indent 1
        lbrace
        (group (separate_map (break 1) (doc_of_binding cs) bs))
        rbrace

    | Null ->
      string "null"

  let print chan (expr, cs) =
    ToChannel.pretty 0.7 !out_width chan (doc_of_expr cs expr)

end

include PrettyPrinter
