open Nix.Lexer

let main () =
  let lexbuf = stdin
               |> Lexing.from_channel
               |> (set_filename "<stdin>")
  in
  let rec loop acc =  function
    | [EOF], _ ->  ()
    | xs, stack ->  xs
                    |> List.map print_token
                    |> List.iter print_endline;
      loop ((List.rev xs) @ acc) (tokens stack lexbuf)
  in
  try
    loop [] (tokens [] lexbuf)

  with
    Error msg ->
    print_endline msg

let () = main ()
