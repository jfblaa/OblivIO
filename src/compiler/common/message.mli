module V = Value
module L = Level

type message
  = Relay of {sender: string; receiver: string; channel: string; level: L.level; value: V.value}
  | Greet of {sender: string}
  | Kill

val to_string_at_level: message -> L.level -> string
