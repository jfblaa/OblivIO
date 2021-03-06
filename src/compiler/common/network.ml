open Level
open Value

type lvalue = value * level
             (* from    to     header   contents *)
type message = level * level * lvalue * lvalue
