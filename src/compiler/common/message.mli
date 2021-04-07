module L = Level
module V = Value
module Ty = Types

type packet = Dummy of int | Real of V.value

type message
  = Msg of {sender: string; channel: string; level: L.level; packet: packet}

val to_string: message -> string
val to_string_at_level: message -> L.level -> string
