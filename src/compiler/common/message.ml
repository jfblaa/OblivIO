module L = Level
module V = Value
module Ty = Types

type packet = Dummy of Ty.basetype * int | Real of Ty.basetype * V.value

type message
  = Msg of {sender: L.level; recipient: L.level; tag: string; packet: packet}

let to_string (Msg {sender; recipient; tag; packet}) =
  String.concat ""
  [ L.to_string sender
  ; " --("
  ; tag
  ; ", "
  ; (match packet with
    | Real (ty, v) -> Ty.base_to_string ty ^ " " ^ V.to_string v
    | Dummy (ty,m) -> Ty.base_to_string ty ^  " N/A" ^ V.size_to_string m)
  ; ")--> "
  ; L.to_string recipient
  ]

let to_string_at_level (Msg {sender; recipient; tag; packet;_} as msg) level =
  let ty,m = match packet with Real (ty,(_,m)) -> ty,m | Dummy (ty,m) -> ty,m in
  if L.flows_to sender level || L.flows_to recipient level
  then to_string msg
  else String.concat ""
    [ L.to_string sender
    ; " --("
    ; tag
    ; ", "
    ; Ty.base_to_string ty
    ; " ???"
    ; V.size_to_string m
    ; ")--> "
    ; L.to_string recipient
    ]
