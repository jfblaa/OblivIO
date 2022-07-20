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

let bitAux (Lbit{bit;level}) lvlOpt =
  match lvlOpt with
  | Some lvl when not (L.flows_to level lvl) -> "?"
  | _ -> Int.to_string bit

let valueAux (Lval{value;level}) lvlOpt =
  match lvlOpt with
  | Some lvl when not (L.flows_to level lvl) -> "?"
  | _ -> V.to_string value


let sizeOfLvalue (Lval{value;_}) =
  V.size value

let sizeOfMsgValue = function
  | Relay {lvalue;_} ->
    sizeOfLvalue lvalue
  | _ -> 0

let to_string ?(lvlOpt=None) msg =
  match msg with
  | Relay {sender;channel;lbit;lvalue;_} ->
    String.concat ""
    [ sender
    ; "->"
    ; C.to_string channel
    ; " {"
    ; bitAux lbit lvlOpt
    ; ","
    ; valueAux lvalue lvlOpt
    ; "} ("
    ; Int.to_string @@ sizeOfLvalue lvalue
    ; " bytes)"
    ]
  | Greet {sender} -> "init: " ^ sender
  | Goodbye {sender} -> "exit: " ^ sender
