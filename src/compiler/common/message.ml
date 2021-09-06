module V = Value
module L = Level

type message
  = Relay of {sender: string; receiver: string; channel: string; level: L.level; value: V.value}
  | Greet of {sender: string}
  | Kill

let to_string_at_level msg lvl =
  match msg with
  | (Relay {sender;receiver;channel;level;value}) ->
    String.concat " "
    [ sender
    ; "->"
    ; receiver
    ; ":"
    ; channel
    ; (if L.flows_to level lvl
      then V.to_string value
      else V.to_string_enc value)
    ]
  | _ -> "N/A message"
