module Nix = struct
  module Ast = Types

  exception ParseError of string

  let parse (chan:in_channel) (file_name:string) =
    let open Parser.MenhirInterpreter in
    let lexbuf = Lexer.set_filename file_name (Lexing.from_channel chan) in
    let q, s = Queue.create (), ref [] in
    let rec next_token lexbuf =
      match Lexer.next_token q s lexbuf with
      | SCOMMENT s | MCOMMENT s ->
        Printf.eprintf "comment '%s'\n" s;
        next_token lexbuf
      | token ->
        token
    in
    let rec go state =
      match state with
      | InputNeeded _ ->
        let token_and_pos = (next_token lexbuf,
                             lexbuf.lex_start_p,
                             lexbuf.lex_curr_p)
        in go (offer state token_and_pos)
      | Shifting _ | AboutToReduce _ ->
        go (resume state)
      | HandlingError _ | Rejected ->
        let msg = Printf.sprintf
            "parse error at: %s\n" (Lexer.print_position lexbuf)
        in raise (ParseError msg)
      | Accepted expr ->
        expr
    in
    try
      go (Parser.Incremental.main lexbuf.lex_curr_p)
    with
    | Lexer.Error msg ->
      raise (ParseError (Printf.sprintf "lexing error: %s\n" msg))
    | exc ->
      raise exc
end

include Nix
