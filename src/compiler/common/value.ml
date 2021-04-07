
type basevalue =
| StringVal of string
| IntVal of int

type value = 
  | Regular of basevalue * int
  | Obliv of bool * basevalue * int

let size_of_base = function
  | IntVal _ -> 64
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

let to_string = function
  | Regular (v,z) -> base_to_string v ^ size_to_string z
  | Obliv (b,v,z) -> "(" ^ Bool.to_string b ^ ", " ^ base_to_string v ^ size_to_string z ^ ")"
