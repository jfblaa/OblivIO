
module L = Level

type base = 
  | INT
  | STRING of L.level
  | ERROR

type ty = Type of {base: base; level: L.level}

val base: ty -> base
val level: ty -> L.level

val to_string: ty -> string
val base_to_string: base -> string
