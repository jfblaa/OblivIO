type oper 
  = PlusOp | MinusOp | TimesOp | DivideOp
  | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp 
  | AndOp | OrOp
  | CaretOp

val to_string : oper -> string