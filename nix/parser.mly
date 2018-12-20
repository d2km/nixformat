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
%token <string> STR_END
%token <string> ID

/* Tokens that stand for themselves */
%token WS
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
%token ANTIQUOTE "${"
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
%token COMA ","

/* Keywords */
%token IMPORT "import"
%token WITH "with"
%token REC "rec"
%token LET "let"
%token IN "in"
%token INHERIT "inherit"
%token NULL "null"

%start <Types.expr> main

%{
  open Types
%}

%%

main:
| e = expr EOL
    { e }

expr:
| l = expr "+" r = term
    { Op(Plus, l, r) }
| l = expr "-" r = term
    { Op(Minus, l, r) }
| t = term
    { t }

term:
| l = term "*" r = factor
    { Op(Mult, l, r) }
| l = term "/" r = factor
    { Op(Div, l, r) }
| f = factor
    { f }

factor:
/* | "-" f = expr %prec UMINUS */
/*     { - e }  */
| "(" e = expr ")"
    { e }
| i = INT
    { Val i }
