
module L = Level

type basetype = 
  | INT
  | STRING
  | ERROR

type ty = Type of {base: basetype; level: L.level}

let base (Type{base;_}) = base
let level (Type{level;_}) = level

let base_to_string = function
  | INT -> "int"
  | STRING -> "string"
  | ERROR -> "error"

let to_string (Type{base;level}) =
  String.concat ""
    [base_to_string base; "@"; L.to_string level]
