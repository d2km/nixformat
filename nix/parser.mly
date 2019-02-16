/* Tokens with data */
%token <string> INT
%token <string> FLOAT
/* a path */
%token <string> PATH
/* search path, enclosed in <> */
%token <string> SPATH
/* home path, starts with ~ */
%token <string> HPATH
%token <string> URI
%token <string> BOOL
%token <string> STR_START
%token <string> STR_MID
%token STR_END
%token <string> ISTR_START
%token <string> ISTR_MID
%token <int> ISTR_END
%token <string> ID
/* %token <string> SCOMMENT */
/* %token <string> MCOMMENT */
/* Tokens that stand for themselves */
%token SELECT "."
%token QMARK "?"
%token CONCAT "++"
%token NOT "!"
%token MERGE "//"
%token ASSIGN "="
%token LT "<"
%token LTE "<="
%token GT ">"
%token GTE ">="
%token EQ "=="
%token NEQ "!="
%token AND "&&"
%token OR "||"
%token IMPL "->"
%token AQUOTE_OPEN "${"
%token AQUOTE_CLOSE "}$"
%token LBRACE "{"
%token RBRACE "}"
%token LBRACK "["
%token RBRACK "]"
%token PLUS "+"
%token MINUS "-"
%token TIMES "*"
%token SLASH "/"
%token LPAREN "("
%token RPAREN ")"
%token COLON ":"
%token SEMICOLON ";"
%token COMMA ","
%token ELLIPSIS "..."
%token AS "@"
/* Keywords */
%token WITH "with"
%token REC "rec"
%token LET "let"
%token IN "in"
%token INHERIT "inherit"
%token NULL "null"
%token IF "if"
%token THEN "then"
%token ELSE "else"
%token ASSERT "assert"
%token ORDEF "or"
/* A special token to denote {} */
%token EMPTY_CURLY "{}"

/* end of input */
%token <Comments.t Queue.t> EOF

%{
  open Types
%}

%start <Types.expr * Comments.t Queue.t> main

%%

main:
| e = expr0; cs = EOF
    { (e, cs) }

expr0:
| "if"; e1 = expr0; "then"; e2 = expr0; "else"; e3 = expr0
    { Cond(e1, e2, e3, $sloc) }
| "with"; e1 = expr0; ";"; e2 = expr0
    { With(e1, e2, $sloc) }
| "assert"; e1 = expr0; ";"; e2 = expr0
    { Assert(e1, e2, $sloc) }
| "let"; xs = nonempty_list(binding); "in"; e = expr0
    { Let(xs, e, $sloc) }
| l = lambda
    { Val (l, $sloc) }
| e = expr1
    { e }

/*
   rules expr1-expr14 are almost direct translation of the operator
   precedence table:
   https://nixos.org/nix/manual/#sec-language-operators
 */

%inline binary_expr(Lhs, Op, Rhs):
lhs = Lhs; op = Op; rhs = Rhs
    { BinaryOp(op, lhs, rhs, $sloc) }

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
    { UnaryOp(Not, e, $sloc) }
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
    { Test(e, p, $sloc) }
| e = expr12
    { e }

expr12:
| e = preceded("-", expr13)
    { UnaryOp(Negate, e, $sloc) }
| e = expr13
    { e }

expr13:
| f = expr13; arg = expr14
    { Apply(f, arg, $sloc) }
| e = expr14
    { e }

%inline selectable:
| s = set
    { Val (s, $sloc) }
| id = ID
    { Id (id, $sloc) }
| e = delimited("(", expr0, ")")
    { e }

expr14:
| e = selectable; "."; p = attr_path; o = option(preceded("or", expr14))
    { Select(e, p, o, $sloc) }
| e = atomic_expr
    { e }

atomic_expr:
| id = ID
    { Id (id, $sloc) }
| v = value
    { Val (v, $sloc) }
| e = delimited("(", expr0, ")")
    { e }

attr_path:
| p = separated_nonempty_list(".", attr_path_component)
    { p }

attr_path_component:
| id = ID
    {Id (id, $sloc)}
| e = delimited("${", expr0, "}$")
    { Aquote (e, $sloc) }
| s = str
    { Val (s, $sloc) }

value:
| s = str
    { s }
| s = istr
    { s }
| i = INT
    {Int i}
| f = FLOAT
    { Float f }
| p = PATH
    { Path p }
| sp = SPATH
    { SPath sp }
| hp = HPATH
    { HPath hp }
| uri = URI
    { Uri uri }
| b = BOOL
    { Bool b }
| "null"
    { Null }
| l = nixlist
    { l }
| s = set
    { s }

%inline str_mid(X):
xs = list(pair(delimited("${", expr0, "}$"), X)) { xs }

/* double-quoted string */
str:
start = STR_START; mids = str_mid(STR_MID); STR_END
    { Str(start, mids) }

/* indented string */
istr:
start = ISTR_START; mids = str_mid(ISTR_MID); i = ISTR_END
    { IStr(i, start, mids) }

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
    { let (k, v) = kv in AttrPath(k, v, $sloc) }
| xs = delimited("inherit", pair(option(delimited("(", expr0, ")")), list(ID)), ";")
    { let (prefix, ids) = xs in Inherit(prefix, ids, $sloc) }

lambda:
| id = ID; "@"; p = param_set; ":"; e = expr0
    { Lambda(AliasedSet(id, p), e) }
| p = param_set; "@"; id = ID; ":"; e = expr0
    { Lambda(AliasedSet(id, p), e) }
| p = param_set; ":"; e = expr0
    { Lambda(ParamSet p, e) }
| id = ID; ":"; e = expr0
    { Lambda(Alias id, e) }


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
    { let (id, opt) = p in (id, opt, $sloc) }
