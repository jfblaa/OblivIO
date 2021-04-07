
type basevalue =
| StringVal of string
| IntVal of int

type value = 
  | Regular of basevalue * int
  | Obliv of bool * basevalue * int

val size_of_base: basevalue -> int

val default_base_int: basevalue
val default_base_string: basevalue

val to_string: value -> string
val base_to_string: basevalue -> string
val size_to_string: int -> string
