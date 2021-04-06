
module L = Level

type basetype = 
  | INT
  | STRING
  | OBLIV of basetype
  | ERROR

type ty = Type of {base: basetype; level: L.level}

let base (Type{base;_}) = base
let level (Type{level;_}) = level

let rec base_to_string = function
  | INT -> "int"
  | STRING -> "string"
  | OBLIV ty -> "obliv " ^ base_to_string ty
  | ERROR -> "error"

let to_string (Type{base;level}) =
  String.concat ""
    [base_to_string base; "@"; L.to_string level]
