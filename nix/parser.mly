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
/* end of input */
%token EOF

%{
  open Types
%}

%start <Types.expr> main

%nonassoc "->"
%left "||"
%left "&&"
%nonassoc "==" "!="
%nonassoc "<" ">" "<=" ">="
%right "//"
%left "-" "+"
%left "*" "/"
%right "++"

%%

main:
| e = expr EOF
    { e }

expr:
| "if" c = expr "then" l = expr "else" r = expr
    { Cond(c, l, r) }
| "with" w = expr ";" e = expr
    { With(w, e) }
| "assert" e1 = expr ";" e2 = expr
    { Assert(e1, e2) }
| "let" xs = nonempty_list(binding) "in" e = expr
    { Let(xs, e) }
| e = op_expr
    { e }
| e = select_expr
    { e }
| e = test_expr
    { e }
| f = atomic_expr; args = nonempty_list(atomic_expr)
    { Apply(f, args) }

select_expr:
| s = atomic_expr "." p = attr_path o = option(preceded("or", atomic_expr))
    { Select(s, p, o) }


attr_path:
| p = separated_nonempty_list(".", attr_path_component)
    { p }

attr_path_component:
| id = ID
    {Id id}
| e = delimited("${", expr, "}$")
    { e }

%inline bin_op:
| "<" {Lt}
| ">" {Gt}
| "<=" {Lte}
| ">=" {Gte}
| "+" {Plus}
| "-" {Minus}
| "*" {Mult}
| "/" {Div}
| "==" {Eq}
| "!=" {Neq}
| "||" {Or}
| "&&" {And}
| "->" {Impl}
| "//" {Merge}
| "++" {Concat}

op_expr:
| lhs = expr; op = bin_op; rhs = expr
    { BinaryOp(op, lhs, rhs) }
| "-" e = atomic_expr
    { UnaryOp(Negate, e) }
| "!" e = atomic_expr
    { UnaryOp(Not, e) }
| e = atomic_expr
    { e }

%inline testable:
| s = set
    { Val s }
| e = delimited("(", expr, ")")
    { e }

test_expr:
e = testable "?" q = test_query
    { Test(e, q) }

/* by experimenting with the Nix repl, the query can be either an ID, or an
antiquotation, or a double-quoted string */
test_query:
| id = ID
    {Id id}
| e = delimited("${", expr, "}$")
    { e }
| s = str
    { Val s }


atomic_expr:
| id = ID
    { Id id }
| v = value
    { Val v }
| e = delimited("(", expr, ")")
    { e }

%inline str_mid(X):
| xs = list(pair(delimited("${", expr, "}$"), X)) { xs }

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
| l = lambda
    { l }
| "null"
    { Null }
| l = nixlist
    { l }
| s = set
    { s }

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
xs = delimited("[", list(atomic_expr), "]")
    { List xs }

set:
| xs = delimited("{", list(attr), "}")
    { AttSet xs }
| xs = preceded("rec", delimited("{", list(attr), "}"))
    { RecAttSet xs }

attr:
| idk = terminated(separated_pair(ID, "=", atomic_expr), ";")
    { let (k, v) = idk in IdKey(k, v) }
| sk = terminated(separated_pair(str, "=", atomic_expr), ";")
    { let (k, v) = sk in StrKey(k, v) }
| "inherit" prefix = option(delimited("(", ID, ")")) ids = list(ID) ";"
    { Inherit(prefix, ids) }

binding:
kv = separated_pair(ID, "=", expr) ";"
    { kv }

lambda:
| id = ID "@" p = param_set ":" e = expr
    { Lambda(AliasedSet(id, p), e) }
| p = param_set "@" id = ID ":" e = expr
    { Lambda(AliasedSet(id, p), e) }
| p = param_set ":" e = expr
    { Lambda(ParamSet p, e) }
| id = ID ":" e = expr
    { Lambda(Alias id, e) }

param_set:
| xs = delimited("{", separated_list(",", param), "}")
    { CompleteSet xs }
| "{" xs = separated_nonempty_list(",", param) "," "..." "}"
    { IncompleteSet xs }

param:
id = ID; v = option(preceded("?", value))
    { (id, v) }
