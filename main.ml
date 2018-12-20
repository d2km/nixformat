open Nix.Lexer

let main () =
  let lexbuf = stdin
               |> Lexing.from_channel
               |> (set_filename "<stdin>")
  in
  let rec loop acc =  function
    | EOF   ->  print_token EOF :: acc |> List.rev
    | x     ->  loop (print_token x :: acc) (token lexbuf)
  in
  try
    loop [] (token lexbuf)
    |> String.concat "\n"
    |> print_endline
  with
    Error msg ->
    print_endline msg

let () = main ()
