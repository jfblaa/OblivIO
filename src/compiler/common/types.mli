
module L = Level

type basetype = 
  | INT
  | STRING
  | ERROR

type ty = Type of {base: basetype; level: L.level}

val base: ty -> basetype
val level: ty -> L.level

val to_string: ty -> string
val base_to_string: basetype -> string
