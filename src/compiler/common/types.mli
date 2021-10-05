
module L = Level

type basetype = 
  | INT
  | STRING
  | PAIR of basetype * basetype
  | ARRAY of basetype
  | ERROR

type ty = Type of {base: basetype; level: L.level}
type chty = ChType of {reads: ty; writes: ty option}

val base: ty -> basetype
val level: ty -> L.level

val to_string: ty -> string
val base_to_string: basetype -> string
