type comment =
  {
    value: string;
    start_p: Lexing.position;
    end_p: Lexing.position
  }

type t =
  | SingleLine of comment
  | Inline of comment

let to_string = function
  | SingleLine {value = s; _} -> "#" ^ s
  | Inline {value = s; _} -> "/*" ^ s ^ "*/"
