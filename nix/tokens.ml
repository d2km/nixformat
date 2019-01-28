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
type token_value =
  | Str of string
  | Int of int
  | Empty

type token_payload = {
  value: token_value;
  comments: comments
}

type token' =
  |  INT' of string
  |  FLOAT' of string
  |  PATH' of string
  |  SPATH' of string
  |  HPATH' of string
  |  URI' of string
  |  BOOL' of string
  |  STR_START' of string
  |  STR_MID' of string
  |  STR_END'
  |  ISTR_START' of string
  |  ISTR_MID' of string
  |  ISTR_END' of int
  |  ID' of string
  |  SELECT'
  |  QMARK'
  |  CONCAT'
  |  NOT'
  |  MERGE'
  |  ASSIGN'
  |  LT'
  |  LTE'
  |  GT'
  |  GTE'
  |  EQ'
  |  NEQ'
  |  AND'
  |  OR'
  |  IMPL'
  |  AQUOTE_OPEN'
  |  AQUOTE_CLOSE'
  |  LBRACE'
  |  RBRACE'
  |  LBRACK'
  |  RBRACK'
  |  PLUS'
  |  MINUS'
  |  TIMES'
  |  SLASH'
  |  LPAREN'
  |  RPAREN'
  |  COLON'
  |  SEMICOLON'
  |  COMMA'
  |  ELLIPSIS'
  |  AS'
  |  WITH'
  |  REC'
  |  LET'
  |  IN'
  |  INHERIT'
  |  NULL'
  |  IF'
  |  THEN'
  |  ELSE'
  |  ASSERT'
  |  ORDEF'
  |  EMPTY_CURLY'
  |  EOF'


type token =
  (* Tokens with data *)
  |  INT of token_payload
  |  FLOAT of token_payload
  (* a path *)
  |  PATH of token_payload
  (* search path, enclosed in <> *)
  |  SPATH of token_payload
  (* home path, starts with ~ *)
  |  HPATH of token_payload
  |  URI of token_payload
  |  BOOL of token_payload
  |  STR_START of token_payload
  |  STR_MID of token_payload
  |  STR_END of token_payload
  |  ISTR_START of token_payload
  |  ISTR_MID of token_payload
  |  ISTR_END of token_payload
  |  ID of token_payload
  (* Tokens that stand for themselves *)
  |  SELECT of token_payload
  |  QMARK of token_payload
  |  CONCAT of token_payload
  |  NOT of token_payload
  |  MERGE of token_payload
  |  ASSIGN of token_payload
  |  LT of token_payload
  |  LTE of token_payload
  |  GT of token_payload
  |  GTE of token_payload
  |  EQ of token_payload
  |  NEQ of token_payload
  |  AND of token_payload
  |  OR of token_payload
  |  IMPL of token_payload
  |  AQUOTE_OPEN of token_payload
  |  AQUOTE_CLOSE of token_payload
  |  LBRACE of token_payload
  |  RBRACE of token_payload
  |  LBRACK of token_payload
  |  RBRACK of token_payload
  |  PLUS of token_payload
  |  MINUS of token_payload
  |  TIMES of token_payload
  |  SLASH of token_payload
  |  LPAREN of token_payload
  |  RPAREN of token_payload
  |  COLON of token_payload
  |  SEMICOLON of token_payload
  |  COMMA of token_payload
  |  ELLIPSIS of token_payload
  |  AS of token_payload
  (* Keywords *)
  |  WITH of token_payload
  |  REC of token_payload
  |  LET of token_payload
  |  IN of token_payload
  |  INHERIT of token_payload
  |  NULL of token_payload
  |  IF of token_payload
  |  THEN of token_payload
  |  ELSE of token_payload
  |  ASSERT of token_payload
  |  ORDEF of token_payload
  (* A special token to denote {} *)
  |  EMPTY_CURLY of token_payload
  (* end of input *)
  |  EOF of token_payload


let empty_payload ?(before = []) ?(after = []) () =
  {value = Empty; comments = {before; after}}
let str_payload ?(before = []) ?(after = []) s =
  {value = Str s; comments = {before; after}}
let int_payload ?(before = []) ?(after = []) i =
  {value = Int i; comments = {before; after}}

(* quite ugly code, not sure how to make it any better
   Ideally I'd just have | _ v -> v pattern but it doesn't compile
*)
let get_payload = function
  |  INT v |  FLOAT v |  PATH v |  SPATH v |  HPATH v
  |  URI v |  BOOL v |  STR_START v |  STR_MID v |  STR_END v
  |  ISTR_START v |  ISTR_MID v |  ISTR_END v |  ID v
  |  SELECT v |  QMARK v |  CONCAT v |  NOT v |  MERGE v
  |  ASSIGN v |  LT v |  LTE v |  GT v |  GTE v |  EQ v
  |  NEQ v |  AND v |  OR v |  IMPL v |  AQUOTE_OPEN v
  |  AQUOTE_CLOSE v |  LBRACE v |  RBRACE v |  LBRACK v
  |  RBRACK v |  PLUS v |  MINUS v |  TIMES v |  SLASH v
  |  LPAREN v |  RPAREN v |  COLON v |  SEMICOLON v |  COMMA v
  |  ELLIPSIS v |  AS v |  WITH v |  REC v |  LET v |  IN v
  |  INHERIT v |  NULL v |  IF v |  THEN v |  ELSE v |  ASSERT v
  |  ORDEF v |  EMPTY_CURLY v |  EOF v ->
    v


(* an auxiliary function to create tokens *)
let create ?(before = []) ?(after = []) (tok: token') = match tok with
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
