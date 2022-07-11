module V = Value
module L = Level

type message
  = Relay of {sender: string; receiver: string; channel: string; lbit: lbit; lvalue: lvalue}
  | Greet of {sender: string}
  | Goodbye of {sender: string}
and lbit
  = Lbit of {bit: int; level: L.level}
and lvalue
  = Lval of {value: V.value; level: L.level}

let bitAux (Lbit{bit;level}) lvlOpt =
  match lvlOpt with
  | Some lvl when not (L.flows_to level lvl) -> "#"
  | _ -> Int.to_string bit

let valueAux (Lval{value;level}) lvlOpt =
  match lvlOpt with
  | Some lvl when not (L.flows_to level lvl) -> "###"
  | _ -> V.to_string value

let sizeOfMsgValue = function
  | Relay {lvalue=Lval{value;_};_} ->
    Util.size value
  | _ -> 0

let to_string ?(lvlOpt=None) msg =
  match msg with
  | Relay {sender;receiver;channel;lbit;lvalue;_} ->
    String.concat ""
    [ sender
    ; "->"
    ; receiver
    ; "/"
    ; channel
    ; " {"
    ; bitAux lbit lvlOpt
    ; ","
    ; valueAux lvalue lvlOpt
    ; "}"
    ]
  | Greet {sender} -> "init: " ^ sender
  | Goodbye {sender} -> "exit: " ^ sender
