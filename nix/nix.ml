module Nix = struct
  module Ast = Types
  module Comments = Comments
  module Location = Location

  exception ParseError of string

  let parse (chan:in_channel) (file_name:string) =
    let lexbuf = Lexing.from_channel chan in
    let s = Lexer.create file_name in
    try
      Parser.main (Lexer.next_token s) lexbuf
    with
    | Lexer.Error msg ->
      raise (ParseError (Printf.sprintf "lexing error: %s\n" msg))
    | Parser.Error ->
      let msg = Printf.sprintf
          "parse error at: %s\n" (Lexer.print_position lexbuf)
      in raise (ParseError msg)
end

include Nix
