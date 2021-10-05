
module L = Level

type basetype = 
  | INT
  | STRING
  | PAIR of basetype * basetype
  | ARRAY of basetype
  | ERROR

type ty = Type of {base: basetype; level: L.level}
type chty = ChType of {reads: ty; writes: ty option}

let base (Type{base;_}) = base
let level (Type{level;_}) = level

let rec base_to_string = function
  | INT -> "int"
  | STRING -> "string"
  | PAIR (a,b) ->
    String.concat "" [
      "("
    ; base_to_string a
    ; ","
    ; base_to_string b
    ; ")"
    ]
  | ARRAY t -> 
    base_to_string t ^ "[]"
  | ERROR -> "error"

let to_string (Type{base;level}) =
  String.concat ""
    [base_to_string base; "@"; L.to_string level]
  
  
