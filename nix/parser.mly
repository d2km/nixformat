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
%token <string> STR
%token <string> STR_START
%token <string> STR_MID
%token STR_END
%token <string> ISTR_START
%token <string> ISTR_MID
%token <int> ISTR_END
%token <string> ID
%token <string> SCOMMENT
%token <string> MCOMMENT
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
%token IMPORT "import"
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

%start <expr> main

%nonassoc "->"
%left "||"
%left "&&"
%nonassoc "==" "!="
%nonassoc "<" ">" "<=" ">="
%right "//"
%nonassoc "!"
%left "-" "+"
%left "*" "/"
%right "++"
%nonassoc "?"
%nonassoc NEGATION
%nonassoc "."

%%

main:
| e = expr EOF
    { e }

expr:
| "if" c = expr "then" l = expr "else" r = expr
    { Cond(c, l, r) }
| "with" w = expr ";" e = expr
    { With(w, e) }
| "assert" c = expr ";" e = expr
    { Assert(c, e) }
| "let" xs = nonempty_list(binding) "in" e = expr
    { Let(xs, e) }
| e = expr_op
    { e }
| e = select_expr
    { e }
| fun = atomic_expr; args = list(atomic_expr)
    { Apply(fun, args) }

select_expr:
| id = ID "." path = attr_path
    { Select(Id id, path) }
| e = delimited("(", expr, ")") "." path = attr_path "or" defval = value
    { SelectDef(e, path, defval) }

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
| "!=" {Neq}
| "||" {Or}
| "&&" {And}
| "->" {Impl}
| "//" {Merge}
| "++" {Concat}

expr_op:
| lhs = expr; op = bin_op; rhs = expr
    { BinaryOp(op, lhs, rhs) }
| "-" e = expr %prec UMINUS
    { UnaryOp(Negate, e) }
| "!" e = expr
    { UnaryOp(Not, e) }
| e = atomic_expr

atomic_expr:
| v = value
    { Val v }
| e = delimited("(", expr, ")")
    { e }

%inline str_mid(X):
| xs = list(pair(delimited("${", expr, "}$"), X)) { xs }

value:
| start = STR_START; mids = str_mid(STR_MID); STR_END
    { Str(start, mids) }
| start = ISTR_START; mids = istr_mid(ISTR_MID); i = ISTR_END
    { Str(i, start, mids) }
| i = INT
    {Int i}
| f = FLOAT
    { Float i }
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
| "[" xs = list(expr) "]"
    { List xs }
| "{" xs = list(attr) "}"
    { AttSet xs }
| "rec" "{" xs = list(attr) "}"
    { RecAttSet xs }

attr:
| idk = separated_pair(ID, "=", expr) ";"
    { IdKey idk }
| start = STR_START mids = str_mid(STR_MID) STR_END "=" e = expr ";"
    { StrKey(Str(start, mids), e) }
| "inherit" prefix = option(delimited("(", ID, ")")) ids = list(ID) ";"
    { Inherit(prefix, ids) }

binding:
| kv = separated_pair(ID, "=", expr) ";"
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
| "{" xs = separated_list(",", param) "}"
    { CompleteSet xs }
| "{" xs = separated_nonempty_list(",", param) "," "..." "}"
    { IncompleteSet xs }

param:
| id = ID
    { Param id }
| id = ID "?" v = param_default_value
    { DefaultParam(id, v) }

param_default_value:
| s = STR_START STR_END
   { Str(s, []) }
| i = INT
   { Int i }
| f = FLOAT
   { Float f }
| p = PATH
   { Path p }
| sp = SPATH
   { SPATH sp }
| hp = HPATH
    { HPATH hp }
| uri = URI
    { Uri uri }
| b = BOOL
    { Bool of b }
| l = lambda
    { l }
