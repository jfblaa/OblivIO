
type value =
  | IntVal of int
  | StringVal of {length: int; data: char array}
  | PairVal of value * value
  | ArrayVal of {length: int; data: value array}

val size: value -> int

val to_string: value -> string