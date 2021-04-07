module L = Level
module V = Value
module Ty = Types

type packet = Dummy of int | Real of V.value

type message
  = Msg of {sender: string; channel: string; level: L.level; packet: packet}

let to_string (Msg {sender; channel; packet; _}) =
  String.concat ""
  [ sender
  ; ": "
  ; channel
  ; "("
  ; (match packet with
    | Real v -> V.to_string v
    | Dummy z -> "N/A" ^ V.size_to_string z)
  ; ")"
  ]

let to_string_at_level (Msg {sender; channel; level; packet}) ladv =
  String.concat ""
  [ sender
  ; ": "
  ; channel
  ; "("
  ; (match packet with
    | Real v when L.flows_to level ladv -> V.to_string v
    | Dummy z when L.flows_to level ladv -> "N/A" ^ V.size_to_string z
    | Real (V.Regular (_,z)) -> "---" ^ V.size_to_string z
    | Real (V.Obliv (_,_,z)) -> "---" ^ V.size_to_string z
    | Dummy z -> "---" ^ V.size_to_string z)
  ; ")"
  ]

