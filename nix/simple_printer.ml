open Types

module SimplePrinter : PPRINTER = struct
  let rec print chan = function
    | BinaryOp(op, lhs, rhs) ->
      print chan lhs; print_bop chan op; print chan rhs
    | UnaryOp(op, e) ->
      print_uop chan op; print chan e
    | Cond(e1, e2, e3) ->
      output_string chan "if ";
      print_paren chan e1;
      output_string chan " then ";
      print_paren chan e2;
      output_string chan " else ";
      print_paren chan e3
    | With(e1, e2) ->
      output_string chan "with ";
      print chan e1;
      output_string chan "; ";
      print chan e2
    | Assert(e1, e2) ->
      output_string chan "assert ";
      print chan e1;
      output_string chan "; ";
      print chan e2
    | Test(e1, e2) ->
      print chan e1;
      output_string chan "? ";
      print chan e2
    | Let(bindings, e) ->
      output_string chan "let ";
      List.iter (fun (id, v) ->
          output_string chan (id ^ "=");
          print chan v;
          output_char chan ';'
        ) bindings;
      output_string chan " in ";
      print chan e
    | Val v ->
      print_val chan v
    | Id id ->
      output_string chan id
    | Select(e1, components, defval) ->
      print_path_component chan e1;
      List.iter (fun c ->
          output_char chan '.';
          print_path_component chan c
        ) components;
      (
        match defval with
        | Some e ->
          output_string chan " or ";
          print_paren chan e
        | None ->
          ()
      )

    | Apply(f, args) ->
      print chan f;
      List.iter (fun arg ->
          output_char chan ' ';
          print_paren chan arg
        ) args;

  and print_paren chan e =
    output_char chan '(';
    print chan e;
    output_char chan ')'

  and print_path_component chan = function
    | (Val _) as e -> print chan e
    | (Id _) as e -> print chan e
    | e -> print_paren chan e

  and print_bop chan op =
    output_string chan (match op with
        | Plus -> "+"
        | Minus -> "-"
        | Mult -> "*"
        | Div -> "/"
        | Gt -> ">"
        | Lt -> "<"
        | Lte -> "<="
        | Gte -> ">="
        | Eq -> "=="
        | Neq -> "!="
        | Or -> "||"
        | And -> "&&"
        | Impl -> "->"
        | Merge -> "//"
        | Concat -> "++"
      )

  and print_uop chan op =
    output_char chan (match op with | Negate -> '-' | Not -> '-')

  and print_val chan = function
    | Str(s, mids) -> print_str chan (s, mids)
    | IStr(i, s, mids) -> print_istr chan (i, s, mids)
    | Int i -> output_string chan i
    | Float f -> output_string chan f
    | Path p -> output_string chan p
    | SPath s -> output_string chan s
    | HPath h -> output_string chan h
    | Uri u -> output_string chan u
    | Bool b -> output_string chan b
    | Lambda(p, e) -> print_lam chan (p, e)
    | List es ->
      output_string chan "[";
      List.iter (fun e -> output_char chan ' ';
                  print_paren chan e
                ) es;
      output_string chan " ]"
    | AttSet atts ->
      output_string chan "{ ";
      List.iter (fun att ->
          print_att chan att;
          output_char chan ' '
        ) atts;
      output_char chan '}'
    | RecAttSet atts ->
      output_string chan "rec { ";
      List.iter (fun att ->
          print_att chan att;
          output_char chan ' '
        ) atts;
      output_char chan '}'

    | Null ->
      output_string chan "null"

  and print_str chan (s, mids) =
    output_char chan '"';
    output_string chan s;
    List.iter (fun (e, s) ->
        output_string chan "${";
        print chan e;
        output_char chan '}';
        output_string chan s
      ) mids;
    output_char chan '"'

  and print_istr chan (_, s, mids) =
    output_string chan "''";
    output_string chan s;
    List.iter (fun (e, s) ->
        output_string chan "${";
        print chan e;
        output_char chan '}';
        output_string chan s
      ) mids;
    output_string chan "''"

  and print_lam chan (p, e) =
    (match p with
    | Alias x ->
      output_string chan x
    | ParamSet ps ->
      output_char chan '{';
      print_param_set chan ps;
      output_char chan '}'
    | AliasedSet (id, ps) ->
      output_char chan '{';
      print_param_set chan ps;
      output_string chan "}@ ";
      output_string chan id
    );
    output_string chan ": ";
    print chan e

  and print_param_set chan = function
    | CompleteSet (head :: tail) ->
      print_param chan head;
      List.iter (fun x ->
          output_string chan " ,";
          print_param chan x
        ) tail;
    | CompleteSet [] ->
      ()
    | IncompleteSet (head :: tail) ->
      print_param chan head;
      List.iter (fun x ->
          output_string chan " ,";
          print_param chan x
        ) tail;
      output_string chan ", ..."
    | IncompleteSet [] ->
      output_string chan "..."

  and print_param chan (id, maybe_val) =
    output_string chan id;
    match maybe_val with
    | Some v -> print_val chan v
    | None -> ()

  and print_att chan = function
    | IdKey(k, e) ->
      output_string chan k;
      output_string chan " = ";
      print chan e;
      output_char chan ';'
    | StrKey(v, e) ->
      print_val chan v;
      output_string chan " = ";
      print chan e;
      output_char chan ';'
    | Inherit(maybe_id, ids) ->
      output_string chan "inherit ";
      (
        match maybe_id with
        | Some x ->
          output_char chan '(';
          output_string chan x;
          output_char chan ')'
        | None -> ()
      );
      List.iter (fun x ->
          output_char chan ' ';
          output_string chan x
        ) ids;
      output_char chan ';'
end

include SimplePrinter
