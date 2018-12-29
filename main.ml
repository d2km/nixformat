open Nix.Lexer

let main () =
  let lexbuf = stdin
               |> Lexing.from_channel
               |> (set_filename "<stdin>")
  in
  let q = Queue.create () in
  let s = ref [] in
  let rec loop = function
    | EOF ->  ()
    | token ->
      print_endline (print_token token);
      loop (next_token q s lexbuf)
  in
  try
    loop (next_token q s lexbuf)
  with
    Error msg ->
    print_endline msg

let () = main ()
