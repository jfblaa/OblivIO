module V = Value
module L = Level
module C = Channel

type message
  = Relay of {sender: string; channel: C.channel; lbit: lbit; lvalue: lvalue}
  | Greet of {sender: string}
  | Goodbye of {sender: string}
and lbit
  = Lbit of {bit: int; level: L.level}
and lvalue
  = Lval of {value: V.value; level: L.level}

val sizeOfMsgValue: message -> int

val to_string: ?lvlOpt:(L.level option) -> message -> string
