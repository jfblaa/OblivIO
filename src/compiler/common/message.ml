module V = Value
module Ty = Types

type packet = Dummy of int | Real of V.value

type message
  = Msg of {sender: string; channel: string; packet: packet}

let to_string (Msg {channel; packet; _}) =
  String.concat ""
  [ channel
  ; "("
  ; (match packet with
    | Real v -> V.to_string v
    | Dummy z -> "N/A" ^ V.size_to_string z)
  ; ")"
  ]

let to_string_hidden (Msg {channel; packet;_}) =
  String.concat ""
  [ channel
  ; "("
  ; (match packet with
    | Real (V.Regular (_,z)) -> "---" ^ V.size_to_string z
    | Real (V.Obliv (_,_,z)) -> "---" ^ V.size_to_string z
    | Dummy z -> "---" ^ V.size_to_string z)
  ; ")"
  ]

