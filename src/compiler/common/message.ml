module V = Value
module L = Level

type message
  = Msg of {sender: string; receiver: string; channel: string; level: L.level; value: V.value}

let to_string_at_level (Msg {sender;receiver;channel;level;value}) lvl =
  String.concat " "
  [ sender
  ; "sends"
  ; if L.flows_to level lvl
    then V.to_string value
    else V.to_string_enc value
  ; "to"
  ; receiver
  ; "on channel"
  ; channel
  ]

