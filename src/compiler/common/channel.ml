type channel = Explicit of (string * string) | Implicit of string

let to_string = function
  | Explicit (node,ch) -> node ^ "/" ^ ch
  | Implicit ch -> ch