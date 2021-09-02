type level

val bottom: level
val lub: level -> level -> level
val flows_to: level -> level -> bool
val of_list: string list -> level
val to_string: level -> string

type 'a lvlmap
val empty: 'a lvlmap
val enter: 'a lvlmap * level * 'a -> 'a lvlmap
val look: 'a lvlmap * level -> 'a option
val numItems: 'a lvlmap -> int