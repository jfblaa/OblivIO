
type value =
| IntVal of int
| StringVal of char array
| PairVal of value * value
| ArrayVal of value array

let rec size = function
  | IntVal _ -> 8
  | StringVal s -> Array.length s
  | PairVal (a,b) -> size a + size b
  | ArrayVal arr -> Array.fold_left (fun acc x -> acc + size x) 0 arr

let rec to_string = function
  | StringVal s -> "\"" ^ (s |> Array.to_seq |> String.of_seq) ^ "\""
  | IntVal n -> Int.to_string n
  | PairVal (a,b) -> String.concat "" [
      "("
    ; to_string a
    ; ","
    ; to_string b
    ; ")"
    ]
  | ArrayVal arr ->
    let data =
      arr |> Array.to_list
          |> List.map to_string
          |> String.concat "," in
    "[" ^ data ^ "]"
