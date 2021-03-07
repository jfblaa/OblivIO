module L = Level
module V = Value
module Ty = Types

type packet = Dummy of Ty.basetype * int | Real of Ty.basetype * V.value

type message
  = Msg of {sender: L.level; recipient: L.level; header: string; packet: packet}

val to_string: message -> string
val to_string_at_level: message -> L.level -> string
