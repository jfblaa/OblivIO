type 'a strmap
val empty: 'a strmap
val enter: 'a strmap * string * 'a -> 'a strmap
val look: 'a strmap * string -> 'a option
val numItems: 'a strmap -> int
