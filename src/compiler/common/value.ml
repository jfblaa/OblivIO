
type basevalue =
  StringVal of string
| IntVal of int

type value = basevalue * int

let to_string (base,m) =
  String.concat ""
  [ (match base with
    | StringVal s -> "\"" ^ s ^ "\""
    | IntVal n -> Int.to_string n)
  ; "@"
  ; Int.to_string m
  ]
