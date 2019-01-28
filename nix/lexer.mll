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

(* Lexer state *)
type t = {
  (* token queue *)
  queue: token Queue.t;
  mutable this_line_token: token option;
  (* accumulated tokens *)
  mutable comments: Comments.t list;
  (* braces stack *)
  mutable stack: braces list
}

let create () : t =
  {
    queue = Queue.create ();
    this_line_token = None;
    comments = [];
    stack = []
  }

let take_token s =
  try Some (Queue.take s.queue) with Queue.Empty -> None

(* Add a new token and attach the accumulated comments if any *)
let add_token s tok' =
  let tok = Tokens.create ~before:s.comments tok' in
  Queue.add tok s.queue;
  s.this_line_token <- Some tok;
  s.comments <- []

(* lookup table for one-character tokens *)
let char_table: token' array = Array.make 93 EOF'
let _ =
  List.iter (fun (k, v) -> Array.set char_table ((int_of_char k) - 1) v)
    [
      '.', SELECT';
      '?', QMARK';
      '!', NOT';
      '=', ASSIGN';
      '<', LT';
      '>', GT';
      '[', LBRACK';
      ']', RBRACK';
      '+', PLUS';
      '-', MINUS';
      '*', TIMES';
      '/', SLASH';
      '(', LPAREN';
      ')', RPAREN';
      ':', COLON';
      ';', SEMICOLON';
      ',', COMMA';
      '@', AS'
    ]

(* lookup table for two- and three-character tokens *)
let str_table = Hashtbl.create 10
let _ =
  List.iter (fun (kwd, tok) -> Hashtbl.add str_table kwd tok)
    [
      "//", MERGE';
      "++", CONCAT';
      "<=", LTE';
      ">=", GTE';
      "==", EQ';
      "!=", NEQ';
      "&&", AND';
      "||", OR';
      "->", IMPL';
      "...", ELLIPSIS'
    ]

(* lookup table for keywords *)
let keyword_table = Hashtbl.create 10
let _ =
  List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    [ "with", WITH';
      "rec", REC';
      "let", LET';
      "in", IN';
      "inherit", INHERIT';
      "null", NULL';
      "if" , IF';
      "then", THEN';
      "else", ELSE';
      "assert", ASSERT';
      "or", ORDEF' ]

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
    match take_token s' with
    | Some tok ->
      (
        match tok, s'.stack with
        | (AQUOTE_CLOSE _), [] | (EOF _), _ ->
          Queue.add tok s.queue;
        | _, _ ->
          Queue.add tok s.queue;
          go ()
      )
    | None ->
      lexer s' lexbuf;
      go ()
  in
  s'.stack <- [AQUOTE];
  add_token s' AQUOTE_OPEN';
  go ()

(* helper functions *)
let token_of_str state buf =
  match state with
  | `Start -> STR_START' (Buffer.contents buf)
  | `Mid -> STR_MID' (Buffer.contents buf)

let token_of_istr state buf =
  match state with
  | `Start -> ISTR_START' (Buffer.contents buf)
  | `Mid -> ISTR_MID' (Buffer.contents buf)

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
let char_tokens = ['.' '?' '!' '=' '<' '>' '[' ']' '+' '-' '*' '/' '(' ')' ':' ';' ',' '@']

rule get_tokens s = parse
(* skip whitespaces *)
| [' ' '\t' '\r']
    { get_tokens s lexbuf }
(* increase line count for new lines *)
| '\n'
    {
      Lexing.new_line lexbuf;
      (
        match s.this_line_token with
        | Some tok ->
          let {comments; value = _} as x = get_payload tok in
          x.comments.after <- s.comments @ comments.after;
          s.comments <- [];
          s.this_line_token <- None;
        | None -> ()
      );
      get_tokens s lexbuf
    }
| char_tokens as c
    { add_token s (Array.get char_table ((int_of_char c) - 1)) }
| ("//" | "++" | "<=" | ">=" | "==" | "!=" | "&&" | "||" | "->" | "...") as op
    { add_token s (Hashtbl.find str_table op)}
| digit+ as i
    { add_token s (INT' i)}
| float
    { add_token s (FLOAT' (Lexing.lexeme lexbuf)) }
| path
    { add_token s (PATH' (Lexing.lexeme lexbuf)) }
| '<' (spath as p) '>'
    { add_token s (SPATH'  p) }
| '~' path as p
    { add_token s (HPATH'  p) }
| uri
    { add_token s (URI' (Lexing.lexeme lexbuf)) }
| ("true" | "false") as b
    { add_token s (BOOL' b) }
(* keywords or identifies *)
| ((alpha | '_')+ (alpha_digit | ['_' '\'' '-'])*) as id
    {
      add_token s (
        try Hashtbl.find keyword_table id with Not_found -> ID' id
      );
    }
(* comments *)
| '#' ([^ '\n']* as c)
    { s.comments <- (Comments.SingleLine c) :: s.comments }
| "/*"
    {
      let c = Comments.Inline (comment (Buffer.create 64) lexbuf) in
      s.comments <- c :: s.comments
    }
(* the following three tokens change the braces stack *)
| "${"
    { add_token s AQUOTE_OPEN'; s.stack <- AQUOTE :: s.stack }
| '{'
    {
      add_token s LBRACE'; s.stack <- SET :: s.stack;
      (* some extra ceremony to allow empty parameter sets with comments *)
      get_tokens s lexbuf;

      match Queue.peek s.queue with
      | LBRACE {comments = cs; value = _} ->
        ignore (Queue.take s.queue);
        Queue.add (
          Tokens.create
            ~before:(cs.before @ s.comments)
            ~after:cs.after
            EMPTY_CURLY'
        ) s.queue;
        s.comments <- []
      | _ ->
        add_token s RBRACE'

    }
| '}'
    {
      match s.stack with
      | AQUOTE :: rest ->
        add_token s AQUOTE_CLOSE'; s.stack <- rest
      | SET :: rest ->
        add_token s RBRACE'; s.stack <- rest
      | _ ->
        let pos = print_position lexbuf in
        let err = Printf.sprintf "Unbalanced '}' at %s\n" pos in
        raise (Error err)
    }
(* a double-quoted string *)
| '"'
    { string `Start (Buffer.create 64) s lexbuf }
(* an indented string *)
| "''"
    { istring `Start None (Buffer.create 64) s lexbuf }
(* End of input *)
| eof
    { add_token s EOF' }
(* any other character raises an exception *)
| _
    {
      let pos = print_position lexbuf in
      let ch = Lexing.lexeme lexbuf in
      let err = Printf.sprintf "Unexpected character '%s' at %s\n" ch pos in
      raise (Error err)
    }

(* Nix does not allow nested comments, but it is still handy to lex it
   separately because we can properly increase line count. *)
and comment buf = parse
  | '\n'
    {Lexing.new_line lexbuf; Buffer.add_char buf '\n'; comment buf lexbuf}
  | "*/"
    { Buffer.contents buf }
  | _ as c
    { Buffer.add_char buf c; comment buf lexbuf }

and string state buf s = parse
  | '"'                         (* terminate when we hit '"' *)
    { add_token s (token_of_str state buf); add_token s STR_END' }
  | '\n'
    {
      Lexing.new_line lexbuf;
      Buffer.add_char buf '\n';
      string state buf s lexbuf
    }
  | ("\\n" | "\\r" | "\\t" | "\\\\" | "\\${") as esc
      { Buffer.add_string buf (unescape esc); string state buf s lexbuf }
  | "\\" (_ as c)               (* add the character verbatim *)
      { Buffer.add_char buf c; string state buf s lexbuf }
  | "${"               (* collect all the tokens till we hit the matching '}' *)
    {
      add_token s (token_of_str state buf);
      collect_tokens get_tokens s lexbuf;
      string `Mid (Buffer.create 64) s lexbuf
    }
  | _ as c                  (* otherwise just add the character to the buffer *)
    { Buffer.add_char buf c; string state buf s lexbuf }

and istring state imin buf s = parse
  | "''"
      {
        let indent = match imin with | None -> 0 | Some i -> i in
        add_token s (token_of_istr state buf);
        add_token s (ISTR_END' indent)
      }
  | ('\n' (' '* as ws)) as empty_str
    {
      Lexing.new_line lexbuf;
      Buffer.add_string buf empty_str;
      let ws_count = String.length ws in
      match imin with
      | None ->
        istring state (Some ws_count) buf s lexbuf
      | Some i ->
        istring state (Some (min i ws_count)) buf s lexbuf
    }
  | ("''$" | "'''" | "''\\t" | "''\\r") as esc
      { Buffer.add_string buf (unescape esc); istring state imin buf s lexbuf }
  | "''\\" (_ as c)
      { Buffer.add_char buf c; istring state imin buf s lexbuf }
  | "${"
    {
      add_token s (token_of_istr state buf);
      collect_tokens get_tokens s lexbuf;
      istring `Mid imin (Buffer.create 64) s lexbuf
    }
  | _ as c
    { Buffer.add_char buf c; istring state imin buf s lexbuf }
{

let rec next_token (s:t) (lexbuf: Lexing.lexbuf): token =
  match take_token s with
  | Some token ->
    token
  | None ->
    get_tokens s lexbuf;
    next_token s lexbuf
}
