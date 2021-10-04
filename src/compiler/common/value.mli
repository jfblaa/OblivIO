
type value =
  | IntVal of int
  | StringVal of char array
  | PairVal of value * value
  | ArrayVal of value array

val size: value -> int

val to_string: value -> string