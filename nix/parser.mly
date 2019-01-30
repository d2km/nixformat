%{
  open Types
  open Tokens
%}

/* Tokens with data */
%token <string token_payload> INT FLOAT PATH SPATH HPATH URI BOOL ID
/* Strings */
%token <string token_payload> STR_START STR_MID
%token <unit token_payload> STR_END
/* Indented strings */
%token <string token_payload> ISTR_START ISTR_MID
%token <int token_payload> ISTR_END

/* Tokens that stand for themselves */
%token <unit token_payload> SELECT "."
%token <unit token_payload> QMARK "?"
%token <unit token_payload> CONCAT "++"
%token <unit token_payload> NOT "!"
%token <unit token_payload> MERGE "//"
%token <unit token_payload> ASSIGN "="
%token <unit token_payload> LT "<"
%token <unit token_payload> LTE "<="
%token <unit token_payload> GT ">"
%token <unit token_payload> GTE ">="
%token <unit token_payload> EQ "=="
%token <unit token_payload> NEQ "!="
%token <unit token_payload> AND "&&"
%token <unit token_payload> OR "||"
%token <unit token_payload> IMPL "->"
%token <unit token_payload> AQUOTE_OPEN "${"
%token <unit token_payload> AQUOTE_CLOSE "}$"
%token <unit token_payload> LBRACE "{"
%token <unit token_payload> RBRACE "}"
%token <unit token_payload> LBRACK "["
%token <unit token_payload> RBRACK "]"
%token <unit token_payload> PLUS "+"
%token <unit token_payload> MINUS "-"
%token <unit token_payload> TIMES "*"
%token <unit token_payload> SLASH "/"
%token <unit token_payload> LPAREN "("
%token <unit token_payload> RPAREN ")"
%token <unit token_payload> COLON ":"
%token <unit token_payload> SEMICOLON ";"
%token <unit token_payload> COMMA ","
%token <unit token_payload> ELLIPSIS "..."
%token <unit token_payload> AS "@"
/* Keywords */
%token <unit token_payload> WITH "with"
%token <unit token_payload> REC "rec"
%token <unit token_payload> LET "let"
%token <unit token_payload> IN "in"
%token <unit token_payload> INHERIT "inherit"
%token <unit token_payload> NULL "null"
%token <unit token_payload> IF "if"
%token <unit token_payload> THEN "then"
%token <unit token_payload> ELSE "else"
%token <unit token_payload> ASSERT "assert"
%token <unit token_payload> ORDEF "or"
/* A special token to denote {} */
%token <unit token_payload> EMPTY_CURLY "{}"

/* end of input */
%token <unit token_payload> EOF

%start <Types.expr> main

%%

main:
| e = expr0 EOF
    { e }

expr0:
| "if"; e1 = expr0; "then"; e2 = expr0; "else"; e3 = expr0
    { Cond(e1, e2, e3) }
| "with"; e1 = expr0; ";"; e2 = expr0
    { With(e1, e2) }
| "assert"; e1 = expr0; ";"; e2 = expr0
    { Assert(e1, e2) }
| "let"; xs = nonempty_list(binding); "in"; e = expr0
    { Let(xs, e) }
| l = lambda
    { Val l }
| e = expr1
    { e }

/*
   rules expr1-expr14 are almost direct translation of the operator
   precedence table:
   https://nixos.org/nix/manual/#sec-language-operators
 */

%inline binary_expr(Lhs, Op, Rhs):
lhs = Lhs; op = Op; rhs = Rhs
    { BinaryOp(op, lhs, rhs) }

expr1:
| e = binary_expr(expr2, "->" {Impl}, expr1)
| e = expr2
    { e }

expr2:
| e = binary_expr(expr2, "||" {Or}, expr3)
| e = expr3
    { e }

expr3:
| e = binary_expr(expr3, "&&" {And}, expr4)
| e = expr4
    { e }

%inline expr4_ops:
| "==" {Eq}
| "!=" {Neq}

expr4:
| e = binary_expr(expr5, expr4_ops, expr5)
| e = expr5
    { e }

%inline expr5_ops:
| "<" {Lt}
| ">" {Gt}
| "<=" {Lte}
| ">=" {Gte}

expr5:
| e = binary_expr(expr6, expr5_ops, expr6)
| e = expr6
    { e }

expr6:
| e = binary_expr(expr7, "//" {Merge}, expr6)
| e = expr7
    { e }

expr7:
| e = preceded("!", expr7)
    { UnaryOp(Not, e) }
| e = expr8
    { e }

%inline expr8_ops:
| "+" {Plus}
| "-" {Minus}

expr8:
| e = binary_expr(expr8, expr8_ops, expr9)
| e = expr9
    { e }

%inline expr9_ops:
| "*" {Mult}
| "/" {Div}

expr9:
| e = binary_expr(expr9, expr9_ops, expr10)
| e = expr10
    { e }

expr10:
| e = binary_expr(expr11, "++" {Concat}, expr10)
| e = expr11
    { e }

expr11:
| e = expr12 "?" p = attr_path
    { Test(e, p) }
| e = expr12
    { e }

expr12:
| e = preceded("-", expr13)
    { UnaryOp(Negate, e) }
| e = expr13
    { e }

expr13:
| f = expr13; arg = expr14
    { Apply(f, arg) }
| e = expr14
    { e }

%inline selectable:
| s = set
    { Val s }
| id = ID
    { Id (value_of id) }
| e = delimited("(", expr0, ")")
    { e }

expr14:
| e = selectable; "."; p = attr_path; o = option(preceded("or", expr14))
    { Select(e, p, o) }
| e = atomic_expr
    { e }

atomic_expr:
| id = ID
    { Id (value_of id) }
| v = value
    { Val v }
| e = delimited("(", expr0, ")")
    { e }

attr_path:
| p = separated_nonempty_list(".", attr_path_component)
    { p }

attr_path_component:
| id = ID
    {Id (value_of id)}
| e = delimited("${", expr0, "}$")
    { Aquote e }
| s = str
    { Val s }

value:
| s = str
    { s }
| s = istr
    { s }
| i = INT
    {Int (value_of i)}
| f = FLOAT
    { Float (value_of f) }
| p = PATH
    { Path (value_of p) }
| sp = SPATH
    { SPath (value_of sp) }
| hp = HPATH
    { HPath (value_of hp) }
| uri = URI
    { Uri (value_of uri) }
| b = BOOL
    { Bool (value_of b) }
| "null"
    { Null }
| l = nixlist
    { l }
| s = set
    { s }

%inline str_mid(X):
xs = list(pair(delimited("${", expr0, "}$"), X))
    { List.map (fun (e, v) -> (e, value_of v)) xs }

/* double-quoted string */
str:
start = STR_START; mids = str_mid(STR_MID); STR_END
    { Str(value_of start, mids) }

/* indented string */
istr:
start = ISTR_START; mids = str_mid(ISTR_MID); i = ISTR_END
    { IStr(value_of i, value_of start, mids) }

/* lists and sets */
nixlist:
xs = delimited("[", list(expr14), "]")
    { List xs }

set:
| "{}"
    { AttSet [] }
| "rec"; "{}"
    { RecAttSet [] }
| xs = delimited("{", nonempty_list(binding), "}")
    { AttSet xs }
| xs = preceded("rec", delimited("{", nonempty_list(binding), "}"))
    { RecAttSet xs }

binding:
| kv = terminated(separated_pair(attr_path, "=", expr0), ";")
    { let (k, v) = kv in AttrPath(k, v) }
| xs = delimited("inherit", pair(option(delimited("(", expr0, ")")), list(ID)), ";")
    { let (prefix, ids) = xs in Inherit(prefix, List.map value_of ids) }

lambda:
| id = ID; "@"; p = param_set; ":"; e = expr0
    { Lambda(AliasedSet(value_of id, p), e) }
| p = param_set; "@"; id = ID; ":"; e = expr0
    { Lambda(AliasedSet(value_of id, p), e) }
| p = param_set; ":"; e = expr0
    { Lambda(ParamSet p, e) }
| id = ID; ":"; e = expr0
    { Lambda(Alias (value_of id), e) }


%inline param_set:
| "{}"
    { ([], None) }
| ps = delimited("{", params, "}")
    { ps }

params:
| "..."
    { ([], Some ()) }
| p = param
    { ([p], None) }
| p = param; ","; ps = params
    { let (prev, ellipsis) = ps in (p :: prev, ellipsis) }

param:
p = pair(ID, option(preceded("?", expr0)))
    { let (id, oe) = p in (value_of id, oe) }
