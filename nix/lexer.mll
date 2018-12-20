{
  (* open Parser *)

exception Error of string

type token =
  | INT of string
  | FLOAT of string
  | PATH of string
  | SPATH of string
  | HPATH of string
  | URI of string
  | BOOL of string
  | STR of string
  | STR_START of string
  | STR_MID of string
  | STR_END of string
  | ID of string
  | SELECT
  | QMARK
  | CONCAT
  | NOT
  | MERGE
  | ASSIGN
  | LT
  | LTE
  | GT
  | GTE
  | EQ
  | NEQ
  | AND
  | OR
  | IMPL
  | ANTIQUOTE
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | PLUS
  | MINUS
  | TIMES
  | SLASH
  | LPAREN
  | RPAREN
  | COLON
  | SEMICOLON
  | COMA
  | IMPORT
  | WITH
  | REC
  | LET
  | IN
  | INHERIT
  | NULL
  | EOF


let keyword_table = Hashtbl.create 10
let _ =
  List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    [ "import", IMPORT;
      "with", WITH;
      "rec", REC;
      "let", LET;
      "inherit", INHERIT;
      "null", NULL ]

let print_position lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  Printf.sprintf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let set_filename fname (lexbuf: Lexing.lexbuf)  =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_fname = fname }; lexbuf

}

let digit = ['0'-'9']
let float = digit* '.' digit+ ('e' '-'? digit+)?
let alpha = ['a'-'z' 'A'-'Z']
let alpha_digit = alpha|digit
let path_chr = alpha_digit | ['.' '_' '-' '+']
let path = path_chr* ('/' path_chr+)+
let spath = alpha_digit path_chr* ('/' path_chr+)*
let uri_chr = ['%' '/' '?' ':' '@' '&' '=' '+' '$' ',' '-' '_' '.' '!' '~' '*' '\'']
let uri = alpha (alpha_digit | ['+' '-' '.'])* ':' (alpha_digit | uri_chr)+

rule token = parse
| [' ' '\t']
    { token lexbuf }
| '\n'
    { Lexing.new_line lexbuf; token lexbuf }
| '.'
    { SELECT }
| '?'
    { QMARK }
| "++"
    { CONCAT }
| '!'
    { NOT }
| "//"
    { MERGE }
| '='
    { ASSIGN }
| '<'
    { LT }
| "<="
    { LTE }
| '>'
    { GT }
| ">="
    { GTE }
| "=="
    { EQ }
| "!="
    { NEQ }
| "&&"
    { AND }
| "||"
    { OR }
| "->"
    { IMPL }
| "${"
    { ANTIQUOTE }
| '{'
    { LBRACE }
| '}'
    { RBRACE }
| '['
    { LBRACK }
| ']'
    { RBRACK }
| '+'
    { PLUS }
| '-'
    { MINUS }
| '*'
    { TIMES }
| '/'
    { SLASH }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| ':'
    { COLON }
| ';'
    { SEMICOLON }
| ','
    { COMA }
| digit+ as i
    { INT i }
| float
    { FLOAT (Lexing.lexeme lexbuf) }
| path
    { PATH (Lexing.lexeme lexbuf) }
| '<' (spath as p) '>'
    { SPATH  p }
| '~' path as p
    { HPATH  p }
| uri
    { URI (Lexing.lexeme lexbuf) }
| ("true" | "false") as b
    { BOOL b }
| (alpha (alpha_digit | ['_' '\''])*) as id
    {try Hashtbl.find keyword_table id with Not_found -> ID id}
| eof
    { EOF }
| _
    {
      let pos = print_position lexbuf in
      let tok = Lexing.lexeme lexbuf in
      let err = Printf.sprintf "Unexpected character '%s' at %s\n" tok pos in
      raise (Error err)
    }

{
let print_token = function
  | INT s -> Printf.sprintf "INT %s" s
  | FLOAT s -> Printf.sprintf "FLOAT %s" s
  | PATH s -> Printf.sprintf "PATH %s" s
  | SPATH s -> Printf.sprintf "SPATH %s" s
  | HPATH s -> Printf.sprintf "HPATH %s" s
  | URI s -> Printf.sprintf "URI %s" s
  | BOOL s -> Printf.sprintf "BOOL %s" s
  | STR s -> Printf.sprintf "STR %s" s
  | STR_START s -> Printf.sprintf "STR_START %s" s
  | STR_MID s -> Printf.sprintf "STR_MID %s" s
  | STR_END s -> Printf.sprintf "STR_END %s" s
  | ID s -> Printf.sprintf "ID %s" s
  | SELECT -> "SELECT"
  | QMARK -> "QMARK"
  | CONCAT -> "CONCAT"
  | NOT -> "NOT"
  | MERGE -> "MERGE"
  | ASSIGN -> "ASSIGN"
  | LT -> "LT"
  | LTE -> "LTE"
  | GT -> "GT"
  | GTE -> "GTE"
  | EQ -> "EQ"
  | NEQ -> "NEQ"
  | AND -> "AND"
  | OR -> "OR"
  | IMPL -> "IMPL"
  | ANTIQUOTE -> "ANTIQUOTE"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LBRACK -> "LBRACK"
  | RBRACK -> "RBRACK"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | TIMES -> "TIMES"
  | SLASH -> "SLASH"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | COLON -> "COLON"
  | SEMICOLON -> "SEMICOLON"
  | COMA -> "COMA"
  | IMPORT -> "IMPORT"
  | WITH -> "WITH"
  | REC -> "REC"
  | LET -> "LET"
  | IN -> "IN"
  | INHERIT -> "INHERIT"
  | NULL -> "NULL"
  | EOF -> "EOF"

}
