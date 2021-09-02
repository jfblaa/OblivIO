module V = Value
module L = Level

type message
  = Msg of {sender: string; receiver: string; channel: string; level: L.level; value: V.value}

val to_string_at_level: message -> L.level -> string
