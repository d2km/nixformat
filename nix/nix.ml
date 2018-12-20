module Nix = struct
  (* open Types
   *
   * let print_op (op: op) : string =
   *   match op with
   *   | Plus -> "+"
   *   | Minus -> "-"
   *   | Mult -> "*"
   *   | Div -> "/"
   *
   * let rec print_expr (e: expr) : string =
   *   match e with
   *   | Val s ->
   *     s
   *   | Op (op, lhs, rhs) ->
   *     "(" ^ print_expr lhs ^")" ^ print_op op ^ "(" ^ print_expr rhs ^")"
   *
   * let parse_line (line: string) : expr =
   *   let buf = Lexing.from_string (line ^ "\n") in
   *   Parser.main Lexer.token buf
   *
   * module Parser = Parser *)
  module Lexer = Lexer
end

include Nix
