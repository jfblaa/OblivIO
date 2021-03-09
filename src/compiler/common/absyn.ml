type pos = Lexing.position 
type level = Level.level
type basevalue = Value.basevalue
include Oper

type program = Prog of { level: level; vardecls: vardecl list; init: cmd option; hns: hn list }
and vardecl
  = VarDecl of { sizevar: var; var: var; basevalue: basevalue; level: level; pos: pos }
and hn
  = Hn of { level: level; tag: string; sizevar: var; var: var; cmd: cmd; pos: pos }
and var = string
and exp = Exp of { exp_base: exp_base; pos: pos }
and exp_base
  = IntExp of int
  | StringExp of string
  | VarExp of var
  | OpExp of { left: exp; oper: oper; right: exp  }
and cmd = Cmd of { cmd_base: cmd_base; pos: pos }
and cmd_base
  = AssignCmd of { var: var; exp: exp }
  | SeqCmd of { c1: cmd; c2: cmd }
  | SkipCmd
  | IfCmd of { test: exp; thn: cmd; els: cmd }
  | OblivIfCmd of { test: exp; thn: cmd; els: cmd }
  | WhileCmd of { test: exp; body: cmd }
  | SendCmd of { level: level; tag: string; exp: exp }
  | PrintCmd of { info: string option; exp: exp }
  | MimicCmd of cmd
