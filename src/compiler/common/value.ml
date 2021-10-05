
type value =
| IntVal of int
| StringVal of {length: int; data: char array}
| PairVal of value * value
| ArrayVal of {length: int; data: value array}

let rec size = function
  | IntVal _ -> 8
  | StringVal {data;_} -> Array.length data
  | PairVal (a,b) -> size a + size b
  | ArrayVal {data;_} -> Array.fold_left (fun acc x -> acc + size x) 0 data

let rec to_string = function
  | StringVal {length;data} ->
    "\"" ^ (data |> Array.to_list |> Util.take length |> List.to_seq |> String.of_seq) ^ "\""
  | IntVal n -> Int.to_string n
  | PairVal (a,b) -> String.concat "" [
      "("
    ; to_string a
    ; ","
    ; to_string b
    ; ")"
    ]
  | ArrayVal {length;data} ->
    let datastr =
      data |> Array.to_list
           |> Util.take length
           |> List.map to_string
           |> String.concat "," in
    "[" ^ datastr ^ "]"
