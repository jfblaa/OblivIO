module V = Value
module L = Level
module C = Channel
module T = Types

type message
  = Relay of {sender: string; channel: C.channel; lbit: lbit; tvalue: tvalue}
  | Greet of {sender: string}
  | Goodbye of {sender: string}
and lbit
  = Lbit of {bit: int; level: L.level}
and tvalue
  = TypedVal of {value: V.value; ty: T.ty}

val sizeOfMsgValue: message -> int

val to_string: ?ladv:L.level -> message -> string
