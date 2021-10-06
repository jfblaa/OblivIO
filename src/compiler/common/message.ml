module V = Value
module L = Level

type message
= Relay of {sender: string; receiver: string; channel: string; level: L.level; bit: int; value: V.value}
  | Greet of {sender: string}
  | Kill

let to_string_at_level msg lvl =
  match msg with
  | (Relay {sender;receiver;channel;level;bit;value;_}) ->
    String.concat ""
    [ sender
    ; " -> "
    ; receiver
    ; " : "
    ; channel
    ; " {bit: "
    ; (if L.flows_to level lvl
      then Int.to_string bit
      else "#")
    ; ", size: "
    ; Int.to_string @@ V.size value
    ; ", value: "
    ; (if L.flows_to level lvl
      then V.to_string value
      else "###")
    ; "}"
    ]
  | _ -> "N/A message"
