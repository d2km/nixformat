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
      Printf.printf "%s\n" (print_token token);
      loop (next_token q s lexbuf)
  in
  try
    loop (next_token q s lexbuf);
  with
    Error msg ->
    Printf.eprintf "%s\n" msg

let () = main ()
