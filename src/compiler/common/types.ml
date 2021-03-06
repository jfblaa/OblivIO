
module L = Level

type base = 
  | INT
  | STRING of L.level
  | ERROR

type ty = Type of {base: base; level: L.level}

let base (Type{base;_}) = base
let level (Type{level;_}) = level

let base_to_string = function
  | INT -> "int"
  | STRING lvl -> "string" ^ L.to_string lvl
  | ERROR -> "error"

let to_string (Type{base;level}) =
  String.concat ""
    [base_to_string base; "@"; L.to_string level]
