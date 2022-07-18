type channel = Ch of {node: string; handler: string}
type t = channel

val to_string: channel -> string

val compare: channel -> channel -> int