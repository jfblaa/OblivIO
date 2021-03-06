
type basevalue =
  StringVal of string
| IntVal of int

type value = basevalue * int

val to_string: value -> string
