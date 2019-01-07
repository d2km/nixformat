open Nix

let main () =
  let lexbuf = stdin
               |> Lexing.from_channel
               |> (Lexer.set_filename "<stdin>")
  in
  let q = Queue.create () in
  let s = ref [] in
  try
    let parse_tree = Parser.main (Lexer.next_token q s) lexbuf in
    SimplePrinter.print stdout parse_tree;
    print_newline ()
  with
  | Lexer.Error msg ->
    Printf.eprintf "lexing error: %s\n" msg
  | Parser.Error ->
    Printf.eprintf "parse error at: %s\n" (Lexer.print_position lexbuf)

let () = main ()
