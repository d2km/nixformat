{
open Tokens

exception Error of string

(* Types of curly braces.
   AQUOTE corresponds to the braces for antiquotation, i.e. '${...}'
   and SET to an attribute set '{...}'.
 *)
type braces =
  | AQUOTE
  | SET


type state =
  {
    q: token Queue.t;
    cs: Comments.t Queue.t;
    mutable bs: braces list;
  }

let create () =
  {
    q = Queue.create();
    cs = Queue.create();
    bs = [];
  }

let token_of_str state buf =
  match state with
        | `Start -> STR_START (Buffer.contents buf)
        | `Mid -> STR_MID (Buffer.contents buf)

let token_of_istr state buf =
  match state with
        | `Start -> ISTR_START (Buffer.contents buf)
        | `Mid -> ISTR_MID (Buffer.contents buf)

(* lookup table for one-character tokens *)
let char_table = Array.make 93 (EOF (Queue.create ()))
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
    [ "with", WITH;
      "rec", REC;
      "let", LET;
      "in", IN;
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

let collect_tokens lexer s lexbuf =
  let s' = create () in
  let rec go () =
    match (try Some (Queue.take s'.q) with Queue.Empty -> None) with
    | Some token ->
      (
        match token, s'.bs with
        | AQUOTE_CLOSE, [] ->
          Queue.add AQUOTE_CLOSE s.q
        | (EOF cs), _ ->
          Queue.transfer cs s.cs;
          Queue.add (EOF (Queue.copy s.cs)) s.q
        | _, _ ->
          Queue.add token s.q;
          go ()
      )
    | None ->
      lexer s' lexbuf;
      go ()
  in
  Queue.add AQUOTE_OPEN s.q;
  s'.bs <- [AQUOTE];
  lexer s' lexbuf;
  go ()

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
let scheme = "http" 's'? | "ftp" | "ssh" | "git" | "mirror" | "svn"
let uri = scheme ':' (alpha_digit | uri_chr)+
(* let uri = alpha (alpha_digit | ['+' '-' '.'])* ':' (alpha_digit | uri_chr)+ *)
let char_tokens = ['.' '?' '!' '=' '<' '>' '[' ']' '+' '-' '*' '/' '(' ')' ':' ';' ',' '@']

rule get_tokens s = parse
(* skip whitespeces *)
| [' ' '\t' '\r']
    { get_tokens s lexbuf }
(* increase line count for new lines *)
| '\n'
    { Lexing.new_line lexbuf; get_tokens s lexbuf }
| char_tokens as c
    { Queue.add (Array.get char_table ((int_of_char c) - 1)) s.q }
| ("//" | "++" | "<=" | ">=" | "==" | "!=" | "&&" | "||" | "->" | "...") as op
    { Queue.add (Hashtbl.find str_table op) s.q}
| digit+ as i
    { Queue.add (INT i) s.q }
| float
    { Queue.add (FLOAT (Lexing.lexeme lexbuf)) s.q }
| path
    { Queue.add (PATH (Lexing.lexeme lexbuf)) s.q }
| '<' (spath as p) '>'
    { Queue.add (SPATH  p) s.q }
| '~' path as p
    { Queue.add (HPATH  p) s.q }
| uri
    { Queue.add(URI (Lexing.lexeme lexbuf)) s.q }
| ("true" | "false") as b
    { Queue.add (BOOL b) s.q }
(* keywords or identifies *)
| ((alpha | '_')+ (alpha_digit | ['_' '\'' '-'])*) as id
    { Queue.add (try Hashtbl.find keyword_table id with Not_found -> ID id) s.q }
(* comments *)
| '#' ([^ '\n']* as c)
    {
      Queue.add (
        Comments.SingleLine {
          value = c;
          start_p = Lexing.lexeme_start_p lexbuf;
          end_p = Lexing.lexeme_end_p lexbuf
        }
      ) s.cs;
      get_tokens s lexbuf
    }
| "/*"
    {
      Queue.add (comment (Buffer.create 64) lexbuf) s.cs;
      get_tokens s lexbuf
    }
(* the following three tokens change the braces stack *)
| "${"
    { Queue.add AQUOTE_OPEN s.q; s.bs <- AQUOTE :: s.bs }
| '{'
    { Queue.add LBRACE s.q; s.bs <- SET :: s.bs }
| '}'
    {
      match s.bs with
      | AQUOTE :: rest ->
        Queue.add AQUOTE_CLOSE s.q; s.bs <- rest
      | SET :: rest ->
        Queue.add RBRACE s.q; s.bs <- rest
      | _ ->
        let pos = print_position lexbuf in
        let err = Printf.sprintf "Unbalanced '}' at %s\n" pos in
        raise (Error err)
    }
(* a special token to avoid parser conflicts on param sets and attr sets *)
| '{' [' ' '\r' '\t' '\n']* as ws '}'
  {
    (* change the line number *)
    String.iter (fun c ->
        if c == '\n' then Lexing.new_line lexbuf else ()
      ) ws;
    Queue.add EMPTY_CURLY s.q
  }
(* a double-quoted string *)
| '"'
    { string `Start (Buffer.create 64) s lexbuf }
(* an indented string *)
| "''"
    { istring `Start None (Buffer.create 64) s lexbuf }
(* End of input *)
| eof
    { Queue.add (EOF (Queue.copy s.cs)) s.q }
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
    {
      Comments.Inline {
        value = (Buffer.contents buf);
        start_p = Lexing.lexeme_start_p lexbuf;
        end_p = Lexing.lexeme_end_p lexbuf;
      }
    }
  | _ as c
    { Buffer.add_char buf c; comment buf lexbuf }

and string state buf s = parse
  | '"'                         (* terminate when we hit '"' *)
    { Queue.add (token_of_str state buf) s.q; Queue.add STR_END s.q }
  | '\n'
    { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; string state buf s lexbuf }
  | ("\\n" | "\\r" | "\\t" | "\\\\" | "\\${") as esc
      { Buffer.add_string buf esc; string state buf s lexbuf }
  | ("\\" _) as esc
      { Buffer.add_string buf esc; string state buf s lexbuf }
  | "${"               (* collect all the tokens till we hit the matching '}' *)
    {
      Queue.add (token_of_str state buf) s.q;
      collect_tokens get_tokens s lexbuf;
      string `Mid (Buffer.create 64) s lexbuf
    }
  | _ as c                  (* otherwise just add the character to the buffer *)
    { Buffer.add_char buf c; string state buf s lexbuf }

and istring state imin buf s = parse
  | "''"
      {
        let indent = match imin with | None -> 0 | Some i -> i in
        Queue.add (token_of_istr state buf) s.q;
        Queue.add (ISTR_END indent) s.q
      }
  | ('\n' (' '* as ws)) as empty_prefix
    {
      Lexing.new_line lexbuf;
      Buffer.add_string buf empty_prefix;
      let ws_count = String.length ws in
      match imin with
      | None ->
        istring state (Some ws_count) buf s lexbuf
      | Some i ->
        istring state (Some (min i ws_count)) buf s lexbuf
    }
  | ("''$" | "'''" | "''\\t" | "''\\r") as esc
      { Buffer.add_string buf esc; istring state imin buf s lexbuf }
  | ("''\\" _) as esc
      { Buffer.add_string buf esc; istring state imin buf s lexbuf }
  | "${"
    {
      Queue.add (token_of_istr state buf) s.q;
      collect_tokens get_tokens s lexbuf;
      istring `Mid imin (Buffer.create 64) s lexbuf
    }
  | _ as c
    { Buffer.add_char buf c; istring state imin buf s lexbuf }
{

let rec next_token (s: state) (lexbuf: Lexing.lexbuf) : token =
  match (try Some (Queue.take s.q) with | Queue.Empty -> None) with
  | Some token ->
    token
  | None ->
    get_tokens s lexbuf;
    next_token s lexbuf
}
