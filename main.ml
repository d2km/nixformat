open Nix.Lexer

let main () =
  let lexbuf = stdin
               |> Lexing.from_channel
               |> (set_filename "<stdin>")
  in
  let rec loop acc =  function
    | [], _    ->  ([EOF] @ acc) |> List.rev
    | x, stack ->  loop (x @ acc) (tokens stack lexbuf)
  in
  try
    loop [] (tokens [] lexbuf)
    |> List.map print_token
    |> List.iter print_endline
  with
    Error msg ->
    print_endline msg

let () = main ()
