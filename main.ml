let file_names = ref ([]: string list)
let out = ref stdout

let main () =
  Arg.parse [] (fun x -> file_names := x :: !file_names) "";
  let files = match !file_names with
    | [] -> [stdin, "<stdin>"]
    | names -> List.map (fun n -> open_in n, n) names
  in
  files
  |> List.rev
  |> List.iter (fun (file, name) ->
      try
        Nix.parse file name |> Pretty_printer.print !out;
        output_char !out '\n'
      with
      | Nix.ParseError msg ->
        Printf.eprintf "%s" msg
    )

let () = main ()
