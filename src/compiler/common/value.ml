
type basevalue =
  | IntVal of int
  | StringVal of char array

type value = Val of {bit: int; v: basevalue}

let base_to_string = function
  | StringVal s -> "\"" ^ (s |> Array.to_seq |> String.of_seq) ^ "\""
  | IntVal n -> Int.to_string n

let sizeof (Val{v;_}) =
  match v with
  | IntVal _ -> 8
  | StringVal s -> Array.length s

let to_string (Val{bit;v} as v') =
  String.concat ""
  [ "{bit:"
  ; Int.to_string bit
  ; ", size:"
  ; Int.to_string @@ sizeof v'
  ; ", v:"
  ; base_to_string v
  ; "}"
  ]

let to_string_enc (Val{bit;_} as v') =
  String.concat ""
  [ "{bit:"
  ; Int.to_string bit
  ; ", size:"
  ; Int.to_string @@ sizeof v'
  ; ", v:"
  ; "<#####>"
  ; "}"
  ]
