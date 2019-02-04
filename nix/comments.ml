type comment =
  {
    value: string;
    location: Location.t
  }

type t =
  | SingleLine of comment
  | Inline of comment

let to_string = function
  | SingleLine {value = s; _} -> "#" ^ s
  | Inline {value = s; _} -> "/*" ^ s ^ "*/"
