type oper 
  = PlusOp | MinusOp | TimesOp | DivideOp
  | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp 
  | AndOp | OrOp
  | CaretOp
  | PadOp

  let to_string = function
    | PlusOp -> "+"
    | MinusOp -> "-"
    | TimesOp -> "*"
    | DivideOp -> "/"
    | EqOp -> "="
    | NeqOp -> "<>"
    | LtOp -> "<"
    | LeOp -> "<="
    | GtOp -> ">"
    | GeOp -> ">="
    | AndOp -> "and"
    | OrOp -> "or"
    | CaretOp -> "^"
    | PadOp -> "padto"