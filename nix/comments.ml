type comment =
  | SingleLine of string
  | Inline of string

type t =
  {
    value: comment;
    location: Location.t
  }

let to_string = function
  | {value = (SingleLine s); _} -> "#" ^ s
  | {value = (Inline s); _} -> "/*" ^ s ^ "*/"
