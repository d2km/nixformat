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
  | STR_END
  | ISTR_START of string
  | ISTR_MID of string
  | ISTR_END of int
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
  | COMMA
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
  | ORDEF
  | EOF

(* Types of curly braces.
   AQUOTE corresponds to the braces for antiquotation, i.e. '${...}'
   and SET to an attribute set '{...}'.
 *)
type braces =
  | AQUOTE
  | SET

let token_of_str state buf =
  match state with
        | `Start -> STR_START (Buffer.contents buf)
        | `Mid -> STR_MID (Buffer.contents buf)

let token_of_istr state buf =
  match state with
        | `Start -> ISTR_START (Buffer.contents buf)
        | `Mid -> ISTR_MID (Buffer.contents buf)

(* lookup table for one-character tokens *)
let char_table = Array.make 93 EOF
let _ =
  List.iter (fun (k, v) -> Array.set char_table ((int_of_char k) - 1) v)
    [
      '.', SELECT;
      '?', QMARK;
      '!', NOT;
      '=', ASSIGN;
      '<', LT;
      '>', GT;
      '[', LBRACK;
      ']', RBRACK;
      '+', PLUS;
      '-', MINUS;
      '*', TIMES;
      '/', SLASH;
      '(', LPAREN;
      ')', RPAREN;
      ':', COLON;
      ';', SEMICOLON;
      ',', COMMA;
      '@', AS
    ]

(* lookup table for two- and three-character tokens *)
let str_table = Hashtbl.create 10
let _ =
  List.iter (fun (kwd, tok) -> Hashtbl.add str_table kwd tok)
    [
      "//", MERGE;
      "++", CONCAT;
      "<=", LTE;
      ">=", GTE;
      "==", EQ;
      "!=", NEQ;
      "&&", AND;
      "||", OR;
      "->", IMPL;
      "...", ELLIPSIS
    ]

(* lookup table for keywords *)
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
      "assert", ASSERT;
      "or", ORDEF ]

(* replace an escape sequence by the corresponding character(s) *)
let unescape = function
  | "\\n" -> "\n"
  | "\\r" -> "\r"
  | "\\t" -> "\t"
  | "\\\\" -> "\\"
  | "\\${" -> "${"
  | "''$" -> "$"
  | "$$" -> "$"
  | "'''" -> "''"
  | "''\\t" -> "\t"
  | "''\\r" -> "\r"
  | x ->
    failwith (Printf.sprintf "unescape unexpected arg %s" x)

(* utility functions *)
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
let scheme = "http" 's'? | "ftp" | "ssh" | "git" | "mirror"
let uri = scheme ':' (alpha_digit | uri_chr)+
(* let uri = alpha (alpha_digit | ['+' '-' '.'])* ':' (alpha_digit | uri_chr)+ *)
let char_tokens = ['.' '?' '!' '=' '<' '>' '[' ']' '+' '-' '*' '/' '(' ')' ':' ';' ',' '@']

rule tokens brace_stack = parse
(* skip whitespeces *)
| [' ' '\t']
    { tokens brace_stack lexbuf }
(* increase line count for new lines *)
| '\n'
    { Lexing.new_line lexbuf; tokens brace_stack lexbuf }
| char_tokens as c
    { [Array.get char_table ((int_of_char c) - 1)], brace_stack }
| ("//" | "++" | "<=" | ">=" | "==" | "!=" | "&&" | "||" | "->" | "...") as s
    { [Hashtbl.find str_table s], brace_stack}
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
| ((alpha | '_')+ (alpha_digit | ['_' '\''])*) as id
    { [try Hashtbl.find keyword_table id with Not_found -> ID id], brace_stack}
(* comments *)
| '#' ([^ '\n']* as c)
    { [SCOMMENT c], brace_stack }
| "/*"
    { [comment (Buffer.create 64) lexbuf], brace_stack }
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
(* a double-quoted string *)
| '"'
    { string `Start (Buffer.create 64) lexbuf, brace_stack }
(* an indented string *)
| "''"
    { istring `Start None (Buffer.create 64) lexbuf, brace_stack }
(* End of input *)
| eof
    { [EOF], brace_stack }
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

and string state buf = parse
  | '"'                         (* terminate when we hit '"' *)
    { [token_of_str state buf; STR_END] }
  | '\n'
    { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; string state buf lexbuf }
  | ("\\n" | "\\r" | "\\t" | "\\\\" | "\\${") as s
      { Buffer.add_string buf (unescape s); string state buf lexbuf }
  | "\\" (_ as c)               (* add the character verbatim *)
      { Buffer.add_char buf c; string state buf lexbuf }
  | "${"               (* collect all the tokens till we hit the matching '}' *)
    {
      let rec go = function
        | [AQUOTE_CLOSE], [] ->
          AQUOTE_CLOSE :: (string `Mid (Buffer.create 64) lexbuf)
        | xs, stack ->
          xs @ (go (tokens stack lexbuf))
      in
      token_of_str state buf :: AQUOTE_OPEN :: (go (tokens [AQUOTE] lexbuf))
    }
  | _ as c                  (* otherwise just add the character to the buffer *)
    { Buffer.add_char buf c; string state buf lexbuf }

and istring state imin buf = parse
  | "''"
      {
        match imin with
        | None -> [token_of_istr state buf; (ISTR_END 0)]
        | Some i -> [token_of_istr state buf; (ISTR_END i)]
      }
  | ('\n' (' '* as ws)) as s
    {
      Lexing.new_line lexbuf;
      Buffer.add_string buf s;
      let ws_count = String.length ws in
      match imin with
      | None ->
        istring state (Some ws_count) buf lexbuf
      | Some i ->
        istring state (Some (min i ws_count)) buf lexbuf
    }
  | ("''$" | "$$" | "'''" | "''\\t" | "''\\r") as s
      { Buffer.add_string buf (unescape s); istring state imin buf lexbuf }
  | "''\\" (_ as c)
      { Buffer.add_char buf c; istring state imin buf lexbuf }
  | "${"
    {
      let rec go = function
        | [AQUOTE_CLOSE], [] ->
          AQUOTE_CLOSE :: (istring `Mid imin (Buffer.create 64) lexbuf)
        | xs, stack ->
          xs @ (go (tokens stack lexbuf))
      in
      token_of_istr state buf :: AQUOTE_OPEN :: (go (tokens [AQUOTE] lexbuf))
    }
  | _ as c
    { Buffer.add_char buf c; istring state imin buf lexbuf }
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
  | STR_END -> "STR_END"
  | ISTR_START s -> Printf.sprintf "STR_START %s" s
  | ISTR_MID s -> Printf.sprintf "STR_MID %s" s
  | ISTR_END i -> Printf.sprintf "STR_END %d" i
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
  | COMMA -> "COMMA"
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
  | ORDEF -> "ORDEF"
  | EOF -> "EOF"

}
