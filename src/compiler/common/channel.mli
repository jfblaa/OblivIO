type channel = Explicit of (string * string) | Implicit of string

val to_string: channel -> string