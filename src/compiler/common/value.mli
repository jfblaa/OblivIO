
type basevalue =
  | IntVal of int
  | StringVal of char array

type value = Val of {bit: int; v: basevalue}

val sizeof: value -> int

val base_to_string: basevalue -> string
val to_string: value -> string
val to_string_enc: value -> string