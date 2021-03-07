
type basevalue =
  StringVal of string
| IntVal of int

type value = basevalue * int

let size_of_base = function
  | IntVal _ -> 8
  | StringVal s -> 8 * String.length s

let default_base_int =
  IntVal 0

let default_base_string =
  StringVal ""

let base_to_string base =
  match base with
  | StringVal s -> "\"" ^ s ^ "\""
  | IntVal n -> Int.to_string n

let size_to_string m =
  "<" ^ Int.to_string m ^ ">"

let to_string (base,m) =
  base_to_string base ^ size_to_string m
