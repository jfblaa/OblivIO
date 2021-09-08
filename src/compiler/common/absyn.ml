type pos = Lexing.position 
type level = Level.level

include Oper

type program = Prog of { node: string; decls: decl list; chs: ch list }
and ty
  = IntType of level
  | StringType of level
and decl
  = VarDecl of { ty: ty; var: var; init: exp; pos: pos }
  | ChannelDecl of { ty: ty; node: string; ch: string; pos: pos }
  | InputDecl of {ty: ty; pos: pos }
and ch
  = Ch of { ty: ty; ch: string; var: var; body: cmd; pos: pos }
and var = string
and exp = Exp of { exp_base: exp_base; pos: pos }
and exp_base
  = IntExp of int
  | StringExp of string
  | VarExp of var
  | QuestionExp of exp
  | SizeExp of exp
  | OpExp of { left: exp; oper: oper; right: exp  }
and cmd = Cmd of { cmd_base: cmd_base; pos: pos }
and cmd_base
  = SkipCmd
  | SeqCmd of { c1: cmd; c2: cmd }
  | AssignCmd of { var: var; exp: exp }
  | BindCmd of { var: var; exp: exp }
  | InputCmd of { var: var; default: exp }
  | SendCmd of { node: string; channel: string; exp: exp }
  | IfCmd of { test: exp; thn: cmd; els: cmd }
  | WhileCmd of { test: exp; body: cmd }
  | OblivIfCmd of { test: exp; thn: cmd; els: cmd }
  | PrintCmd of { info: string option; exp: exp }
 