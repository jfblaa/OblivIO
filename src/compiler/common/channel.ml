type channel = Ch of {node: string; handler: string}
type t = channel

let to_string (Ch{node;handler}) = node ^ "/" ^ handler

let compare a b =
  match a,b with
  | (Ch{node=n1;handler=h1}), (Ch{node=n2;handler=h2}) when n1 = n2 -> compare h1 h2
  | (Ch{node=n1;_}), (Ch{node=n2;_}) -> compare n1 n2