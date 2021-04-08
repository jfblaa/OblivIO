module V = Value
module Ty = Types

type packet = Dummy of int | Real of V.value

type message
  = Msg of {sender: string; channel: string; packet: packet}

val to_string: message -> string
val to_string_hidden: message -> string
