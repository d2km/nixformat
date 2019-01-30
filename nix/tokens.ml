module Comments = struct
  type t =
    | SingleLine of string
    | Inline of string

  let to_string = function
    | SingleLine s | Inline s -> s
end

type comments = {
  (* comments that precede a token *)
  mutable before: Comments.t list;
  (* comments that follow the token (on the same line) *)
  mutable after: Comments.t list
}

(* value type of tokens that have a value *)
type _ token_value =
  | Str: string -> string token_value
  | Int: int -> int token_value
  | Empty: unit token_value

type 'a token_payload = {
  value: 'a token_value;
  comments: comments
}

let value_of (type t) (p: t token_payload): t =
  match p with
  | {value = Str s; _} -> s
  | {value = Int i; _} -> i
  | {value = Empty; _} -> ()

let comments_of (p: 't token_payload): comments =
  let {value = _ ; comments = cs} = p in cs

(* an auxiliary type to easy token creation *)
type _ token' =
  |  INT': string -> string token'
  |  FLOAT': string -> string token'
  |  PATH': string -> string token'
  |  SPATH': string -> string token'
  |  HPATH': string -> string token'
  |  URI': string -> string token'
  |  BOOL': string -> string token'
  |  STR_START': string -> string token'
  |  STR_MID': string -> string token'
  |  STR_END': unit token'
  |  ISTR_START': string -> string token'
  |  ISTR_MID': string -> string token'
  |  ISTR_END': int -> int token'
  |  ID': string -> string token'
  |  SELECT': unit token'
  |  QMARK': unit token'
  |  CONCAT': unit token'
  |  NOT': unit token'
  |  MERGE': unit token'
  |  ASSIGN': unit token'
  |  LT': unit token'
  |  LTE': unit token'
  |  GT': unit token'
  |  GTE': unit token'
  |  EQ': unit token'
  |  NEQ': unit token'
  |  AND': unit token'
  |  OR': unit token'
  |  IMPL': unit token'
  |  AQUOTE_OPEN': unit token'
  |  AQUOTE_CLOSE': unit token'
  |  LBRACE': unit token'
  |  RBRACE': unit token'
  |  LBRACK': unit token'
  |  RBRACK': unit token'
  |  PLUS': unit token'
  |  MINUS': unit token'
  |  TIMES': unit token'
  |  SLASH': unit token'
  |  LPAREN': unit token'
  |  RPAREN': unit token'
  |  COLON': unit token'
  |  SEMICOLON': unit token'
  |  COMMA': unit token'
  |  ELLIPSIS': unit token'
  |  AS': unit token'
  |  WITH': unit token'
  |  REC': unit token'
  |  LET': unit token'
  |  IN': unit token'
  |  INHERIT': unit token'
  |  NULL': unit token'
  |  IF': unit token'
  |  THEN': unit token'
  |  ELSE': unit token'
  |  ASSERT': unit token'
  |  ORDEF': unit token'
  |  EMPTY_CURLY': unit token'
  |  EOF': unit token'


type token =
  (* Tokens with data *)
  |  INT of string token_payload
  |  FLOAT of string token_payload
  (* a path *)
  |  PATH of string token_payload
  (* search path, enclosed in <> *)
  |  SPATH of string token_payload
  (* home path, starts with ~ *)
  |  HPATH of string token_payload
  |  URI of string token_payload
  |  BOOL of string token_payload
  |  STR_START of string token_payload
  |  STR_MID of string token_payload
  |  STR_END of unit token_payload
  |  ISTR_START of string token_payload
  |  ISTR_MID of string token_payload
  |  ISTR_END of int token_payload
  |  ID of string token_payload
  (* Tokens that stand for themselves *)
  |  SELECT of unit token_payload
  |  QMARK of unit token_payload
  |  CONCAT of unit token_payload
  |  NOT of unit token_payload
  |  MERGE of unit token_payload
  |  ASSIGN of unit token_payload
  |  LT of unit token_payload
  |  LTE of unit token_payload
  |  GT of unit token_payload
  |  GTE of unit token_payload
  |  EQ of unit token_payload
  |  NEQ of unit token_payload
  |  AND of unit token_payload
  |  OR of unit token_payload
  |  IMPL of unit token_payload
  |  AQUOTE_OPEN of unit token_payload
  |  AQUOTE_CLOSE of unit token_payload
  |  LBRACE of unit token_payload
  |  RBRACE of unit token_payload
  |  LBRACK of unit token_payload
  |  RBRACK of unit token_payload
  |  PLUS of unit token_payload
  |  MINUS of unit token_payload
  |  TIMES of unit token_payload
  |  SLASH of unit token_payload
  |  LPAREN of unit token_payload
  |  RPAREN of unit token_payload
  |  COLON of unit token_payload
  |  SEMICOLON of unit token_payload
  |  COMMA of unit token_payload
  |  ELLIPSIS of unit token_payload
  |  AS of unit token_payload
  (* Keywords *)
  |  WITH of unit token_payload
  |  REC of unit token_payload
  |  LET of unit token_payload
  |  IN of unit token_payload
  |  INHERIT of unit token_payload
  |  NULL of unit token_payload
  |  IF of unit token_payload
  |  THEN of unit token_payload
  |  ELSE of unit token_payload
  |  ASSERT of unit token_payload
  |  ORDEF of unit token_payload
  (* A special token to denote {} *)
  |  EMPTY_CURLY of unit token_payload
  (* end of input *)
  |  EOF of unit token_payload


let empty_payload ?(before = []) ?(after = []) () : unit token_payload =
  {value = Empty; comments = {before; after}}
let str_payload ?(before = []) ?(after = []) s : string token_payload =
  {value = Str s; comments = {before; after}}
let int_payload ?(before = []) ?(after = []) i : int token_payload =
  {value = Int i; comments = {before; after}}

(* quite ugly code, not sure how to make it any better *)
let get_comments = function
  |  INT {value = _; comments=cs}
  |  FLOAT {value = _; comments = cs}
  |  PATH {value = _; comments = cs}
  |  SPATH {value = _; comments = cs}
  |  HPATH {value = _; comments = cs}
  |  URI {value = _; comments = cs}
  |  BOOL {value = _; comments = cs}
  |  STR_START {value = _; comments = cs}
  |  STR_MID {value = _; comments = cs}
  |  ID {value = _; comments = cs}
  |  ISTR_START {value = _; comments = cs}
  |  ISTR_MID {value = _; comments = cs}
  |  ISTR_END {value = _; comments = cs}
  |  STR_END {value = _; comments = cs}
  |  SELECT {value = _; comments = cs}
  |  QMARK {value = _; comments = cs}
  |  CONCAT {value = _; comments = cs}
  |  NOT {value = _; comments = cs}
  |  MERGE {value = _; comments = cs}
  |  ASSIGN {value = _; comments = cs}
  |  LT {value = _; comments = cs}
  |  LTE {value = _; comments = cs}
  |  GT {value = _; comments = cs}
  |  GTE {value = _; comments = cs}
  |  EQ {value = _; comments = cs}
  |  NEQ {value = _; comments = cs}
  |  AND {value = _; comments = cs}
  |  OR {value = _; comments = cs}
  |  IMPL {value = _; comments = cs}
  |  AQUOTE_OPEN {value = _; comments = cs}
  |  AQUOTE_CLOSE {value = _; comments = cs}
  |  LBRACE {value = _; comments = cs}
  |  RBRACE {value = _; comments = cs}
  |  LBRACK {value = _; comments = cs}
  |  RBRACK {value = _; comments = cs}
  |  PLUS {value = _; comments = cs}
  |  MINUS {value = _; comments = cs}
  |  TIMES {value = _; comments = cs}
  |  SLASH {value = _; comments = cs}
  |  LPAREN {value = _; comments = cs}
  |  RPAREN {value = _; comments = cs}
  |  COLON {value = _; comments = cs}
  |  SEMICOLON {value = _; comments = cs}
  |  COMMA {value = _; comments = cs}
  |  ELLIPSIS {value = _; comments = cs}
  |  AS {value = _; comments = cs}
  |  WITH {value = _; comments = cs}
  |  REC {value = _; comments = cs}
  |  LET {value = _; comments = cs}
  |  IN {value = _; comments = cs}
  |  INHERIT {value = _; comments = cs}
  |  NULL {value = _; comments = cs}
  |  IF {value = _; comments = cs}
  |  THEN {value = _; comments = cs}
  |  ELSE {value = _; comments = cs}
  |  ASSERT {value = _; comments = cs}
  |  ORDEF {value = _; comments = cs}
  |  EMPTY_CURLY {value = _; comments = cs}
  |  EOF {value = _; comments = cs} ->
    cs

let print_payload (type t) (p: t token_payload) =
  let dq s = "\"" ^ s ^ "\"" in
  let v = match p with
    | {value = Str s; _} -> "Str " ^ dq s
    | {value = Int i; _} -> "Int " ^ string_of_int i
    | {value = Empty; _} -> "Empty"
  in
  let cs = comments_of p in
  let print_comments xs =
    xs
    |> List.rev
    |> List.map (
      function
      | Comments.SingleLine s -> "SingleLine " ^ dq s
      | Comments.Inline s -> "Inline " ^ dq s
    )
  in
  let before = print_comments cs.before in
  let after = print_comments cs.after in
  let comms = " {before = [" ^ String.concat "; " before ^ "]; " ^
              "after = [" ^ String.concat "; " after ^ "]}"
  in
  " {value = " ^ v ^ "; comments = " ^ comms ^ "} "

let print_token = function
  |  INT v -> "INT" ^ print_payload v
  |  FLOAT v -> "FLOAT" ^ print_payload v
  |  PATH v -> "PATH" ^ print_payload v
  |  SPATH v -> "SPATH" ^ print_payload v
  |  HPATH v -> "HPATH" ^ print_payload v
  |  URI v -> "URI" ^ print_payload v
  |  BOOL v -> "BOOL" ^ print_payload v
  |  STR_START v -> "STR_START" ^ print_payload v
  |  STR_MID v -> "STR_MID" ^ print_payload v
  |  ID v -> "ID" ^ print_payload v
  |  ISTR_START v -> "ISTR_START" ^ print_payload v
  |  ISTR_MID v -> "ISTR_MID" ^ print_payload v
  |  ISTR_END v -> "ISTR_END" ^ print_payload v
  |  STR_END v -> "STR_END" ^ print_payload v
  |  SELECT v -> "SELECT" ^ print_payload v
  |  QMARK v -> "QMARK" ^ print_payload v
  |  CONCAT v -> "CONCAT" ^ print_payload v
  |  NOT v -> "NOT" ^ print_payload v
  |  MERGE v -> "MERGE" ^ print_payload v
  |  ASSIGN v -> "ASSIGN" ^ print_payload v
  |  LT v -> "LT" ^ print_payload v
  |  LTE v -> "LTE" ^ print_payload v
  |  GT v -> "GT" ^ print_payload v
  |  GTE v -> "GTE" ^ print_payload v
  |  EQ v -> "EQ" ^ print_payload v
  |  NEQ v -> "NEQ" ^ print_payload v
  |  AND v -> "AND" ^ print_payload v
  |  OR v -> "OR" ^ print_payload v
  |  IMPL v -> "IMPL" ^ print_payload v
  |  AQUOTE_OPEN v -> "AQUOTE_OPEN" ^ print_payload v
  |  AQUOTE_CLOSE v -> "AQUOTE_CLOSE" ^ print_payload v
  |  LBRACE v -> "LBRACE" ^ print_payload v
  |  RBRACE v -> "RBRACE" ^ print_payload v
  |  LBRACK v -> "LBRACK" ^ print_payload v
  |  RBRACK v -> "RBRACK" ^ print_payload v
  |  PLUS v -> "PLUS" ^ print_payload v
  |  MINUS v -> "MINUS" ^ print_payload v
  |  TIMES v -> "TIMES" ^ print_payload v
  |  SLASH v -> "SLASH" ^ print_payload v
  |  LPAREN v -> "LPAREN" ^ print_payload v
  |  RPAREN v -> "RPAREN" ^ print_payload v
  |  COLON v -> "COLON" ^ print_payload v
  |  SEMICOLON v -> "SEMICOLON" ^ print_payload v
  |  COMMA v -> "COMMA" ^ print_payload v
  |  ELLIPSIS v -> "ELLIPSIS" ^ print_payload v
  |  AS v -> "AS" ^ print_payload v
  |  WITH v -> "WITH" ^ print_payload v
  |  REC v -> "REC" ^ print_payload v
  |  LET v -> "LET" ^ print_payload v
  |  IN v -> "IN" ^ print_payload v
  |  INHERIT v -> "INHERIT" ^ print_payload v
  |  NULL v -> "NULL" ^ print_payload v
  |  IF v -> "IF" ^ print_payload v
  |  THEN v -> "THEN" ^ print_payload v
  |  ELSE v -> "ELSE" ^ print_payload v
  |  ASSERT v -> "ASSERT" ^ print_payload v
  |  ORDEF v -> "ORDEF" ^ print_payload v
  |  EMPTY_CURLY v -> "EMPTY_CURLY" ^ print_payload v
  |  EOF v -> "EOF" ^ print_payload v


(* an auxiliary function to create tokens from token' *)
let create (type t) ?(before = []) ?(after = []) (tok: t token') =
  match tok with
  |  INT' v -> INT (str_payload ~before ~after v)
  |  FLOAT' v -> FLOAT (str_payload ~before ~after v)
  |  PATH' v -> PATH (str_payload ~before ~after v)
  |  SPATH' v -> SPATH (str_payload ~before ~after v)
  |  HPATH' v -> HPATH (str_payload ~before ~after v)
  |  URI' v -> URI (str_payload ~before ~after v)
  |  BOOL' v -> BOOL (str_payload ~before ~after v)
  |  STR_START' v -> STR_START (str_payload ~before ~after v)
  |  STR_MID' v -> STR_MID (str_payload ~before ~after v)
  |  STR_END' -> STR_END (empty_payload ~before ~after ())
  |  ISTR_START' v -> ISTR_START (str_payload ~before ~after v)
  |  ISTR_MID' v -> ISTR_MID (str_payload ~before ~after v)
  |  ISTR_END' v -> ISTR_END (int_payload ~before ~after v)
  |  ID' v -> ID (str_payload ~before ~after v)
  |  SELECT' -> SELECT (empty_payload ~before ~after ())
  |  QMARK' -> QMARK (empty_payload ~before ~after ())
  |  CONCAT' -> CONCAT (empty_payload ~before ~after ())
  |  NOT' -> NOT (empty_payload ~before ~after ())
  |  MERGE' -> MERGE (empty_payload ~before ~after ())
  |  ASSIGN' -> ASSIGN (empty_payload ~before ~after ())
  |  LT' -> LT (empty_payload ~before ~after ())
  |  LTE' -> LTE (empty_payload ~before ~after ())
  |  GT' -> GT (empty_payload ~before ~after ())
  |  GTE' -> GTE (empty_payload ~before ~after ())
  |  EQ' -> EQ (empty_payload ~before ~after ())
  |  NEQ' -> NEQ (empty_payload ~before ~after ())
  |  AND' -> AND (empty_payload ~before ~after ())
  |  OR' -> OR (empty_payload ~before ~after ())
  |  IMPL' -> IMPL (empty_payload ~before ~after ())
  |  AQUOTE_OPEN' -> AQUOTE_OPEN (empty_payload ~before ~after ())
  |  AQUOTE_CLOSE' -> AQUOTE_CLOSE (empty_payload ~before ~after ())
  |  LBRACE' -> LBRACE (empty_payload ~before ~after ())
  |  RBRACE' -> RBRACE (empty_payload ~before ~after ())
  |  LBRACK' -> LBRACK (empty_payload ~before ~after ())
  |  RBRACK' -> RBRACK (empty_payload ~before ~after ())
  |  PLUS' -> PLUS (empty_payload ~before ~after ())
  |  MINUS' -> MINUS (empty_payload ~before ~after ())
  |  TIMES' -> TIMES (empty_payload ~before ~after ())
  |  SLASH' -> SLASH (empty_payload ~before ~after ())
  |  LPAREN' -> LPAREN (empty_payload ~before ~after ())
  |  RPAREN' -> RPAREN (empty_payload ~before ~after ())
  |  COLON' -> COLON (empty_payload ~before ~after ())
  |  SEMICOLON' -> SEMICOLON (empty_payload ~before ~after ())
  |  COMMA' -> COMMA (empty_payload ~before ~after ())
  |  ELLIPSIS' -> ELLIPSIS (empty_payload ~before ~after ())
  |  AS' -> AS (empty_payload ~before ~after ())
  |  WITH' -> WITH (empty_payload ~before ~after ())
  |  REC' -> REC (empty_payload ~before ~after ())
  |  LET' -> LET (empty_payload ~before ~after ())
  |  IN' -> IN (empty_payload ~before ~after ())
  |  INHERIT' -> INHERIT (empty_payload ~before ~after ())
  |  NULL' -> NULL (empty_payload ~before ~after ())
  |  IF' -> IF (empty_payload ~before ~after ())
  |  THEN' -> THEN (empty_payload ~before ~after ())
  |  ELSE' -> ELSE (empty_payload ~before ~after ())
  |  ASSERT' -> ASSERT (empty_payload ~before ~after ())
  |  ORDEF' -> ORDEF (empty_payload ~before ~after ())
  |  EMPTY_CURLY' -> EMPTY_CURLY (empty_payload ~before ~after ())
  |  EOF' -> EOF (empty_payload ~before ~after ())
