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
  | SCOMMENT of string
  | MCOMMENT of string
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
  | AQUOTE_OPEN
  | AQUOTE_CLOSE
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
  | ELLIPSIS
  | AS
  | IMPORT
  | WITH
  | REC
  | LET
  | IN
  | INHERIT
  | NULL
  | IF
  | THEN
  | ELSE
  | ASSERT
  | EOF

(* Types of closing curly braces.

   AQUOTE corresponds to the closing curly brace for
   antiquotation, i.e. ${} and SET to the closing braces for {}.
 *)
type braces =
  | AQUOTE
  | SET

let keyword_table = Hashtbl.create 10
let _ =
  List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    [ "import", IMPORT;
      "with", WITH;
      "rec", REC;
      "let", LET;
      "inherit", INHERIT;
      "null", NULL;
      "if" , IF;
      "then", THEN;
      "else", ELSE;
      "assert", ASSERT ]

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
let alpha_digit = alpha | digit
let path_chr = alpha_digit | ['.' '_' '-' '+']
let path = path_chr* ('/' path_chr+)+
let spath = alpha_digit path_chr* ('/' path_chr+)*
let uri_chr = ['%' '/' '?' ':' '@' '&' '=' '+' '$' ',' '-' '_' '.' '!' '~' '*' '\'']
let uri = alpha (alpha_digit | ['+' '-' '.'])* ':' (alpha_digit | uri_chr)+

rule tokens brace_stack = parse
(* skip whitespeces *)
| [' ' '\t']
    { tokens brace_stack lexbuf }
(* increase line count for new lines *)
| '\n'
    { Lexing.new_line lexbuf; tokens brace_stack lexbuf }
| '.'
    { [SELECT], brace_stack }
| '?'
    { [QMARK], brace_stack }
| "++"
    { [CONCAT], brace_stack }
| '!'
    { [NOT], brace_stack }
| "//"
    { [MERGE], brace_stack }
| '='
    { [ASSIGN], brace_stack }
| '<'
    { [LT], brace_stack }
| "<="
    { [LTE], brace_stack }
| '>'
    { [GT], brace_stack }
| ">="
    { [GTE], brace_stack }
| "=="
    { [EQ], brace_stack }
| "!="
    { [NEQ], brace_stack }
| "&&"
    { [AND], brace_stack }
| "||"
    { [OR], brace_stack }
| "->"
    { [IMPL], brace_stack }
| '['
    { [LBRACK], brace_stack }
| ']'
    { [RBRACK], brace_stack }
| '+'
    { [PLUS], brace_stack }
| '-'
    { [MINUS], brace_stack }
| '*'
    { [TIMES], brace_stack }
| '/'
    { [SLASH], brace_stack }
| '('
    { [LPAREN], brace_stack }
| ')'
    { [RPAREN], brace_stack }
| ':'
    { [COLON], brace_stack }
| ';'
    { [SEMICOLON], brace_stack }
| ','
    { [COMA], brace_stack }
| "..."
    { [ELLIPSIS], brace_stack }
| '@'
    { [AS], brace_stack }
| digit+ as i
    { [INT i], brace_stack }
| float
    { [FLOAT (Lexing.lexeme lexbuf)], brace_stack }
| path
    { [PATH (Lexing.lexeme lexbuf)], brace_stack }
| '<' (spath as p) '>'
    { [SPATH  p], brace_stack }
| '~' path as p
    { [HPATH  p], brace_stack }
| uri
    { [URI (Lexing.lexeme lexbuf)], brace_stack }
| ("true" | "false") as b
    { [BOOL b], brace_stack }
(* keywords or identifies *)
| (alpha (alpha_digit | ['_' '\''])*) as id
    { [try Hashtbl.find keyword_table id with Not_found -> ID id], brace_stack}
(* comments *)
| '#' ([^ '\n']+ as c)
    { [SCOMMENT c], brace_stack }
| "/*"
    { [comment (Buffer.create 100) lexbuf], brace_stack }
(* the following three tokens change the brace_stack *)
| "${"
    { [AQUOTE_OPEN], (AQUOTE :: brace_stack) }
| '{'
    { [LBRACE], (SET :: brace_stack) }
| '}'
    {
      match brace_stack with
      | AQUOTE :: rest ->
        [AQUOTE_CLOSE], rest
      | SET :: rest ->
        [RBRACE], rest
      | _ ->
        let pos = print_position lexbuf in
        let err = Printf.sprintf "Unbalanced '}' at %s\n" pos in
        raise (Error err)
    }
(* End of input *)
| eof
    { [], brace_stack }
(* any other character raises an exception *)
| _
    {
      let pos = print_position lexbuf in
      let tok = Lexing.lexeme lexbuf in
      let err = Printf.sprintf "Unexpected character '%s' at %s\n" tok pos in
      raise (Error err)
    }
(* Nix does not allow nested comments, but it is still handy to lex it
   separately because we can properly increase line count. *)
and comment buf = parse
  | '\n'
    {Lexing.new_line lexbuf; Buffer.add_char buf '\n'; comment buf lexbuf}
  | "*/"
    { MCOMMENT (Buffer.contents buf) }
  | _ as c
    { Buffer.add_char buf c; comment buf lexbuf }

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
  | SCOMMENT s -> Printf.sprintf "SCOMMENT %s" s
  | MCOMMENT s -> Printf.sprintf "MCOMMENT %s" s
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
  | AQUOTE_OPEN -> "AQUOTE_OPEN"
  | AQUOTE_CLOSE -> "AQUOTE_CLOSE"
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
  | ELLIPSIS -> "ELLIPSIS"
  | AS -> "AS"
  | IMPORT -> "IMPORT"
  | WITH -> "WITH"
  | REC -> "REC"
  | LET -> "LET"
  | IN -> "IN"
  | INHERIT -> "INHERIT"
  | NULL -> "NULL"
  | IF -> "IF"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | ASSERT -> "ASSERT"
  | EOF -> "EOF"

}
