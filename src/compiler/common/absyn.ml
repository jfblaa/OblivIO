type pos = Lexing.position 

module T = Types
module L = Level
module Ch = Channel
include Oper

type program = Prog of { node: string; decls: decl list; hls: hl list }
and decl
  = VarDecl of { ty: T.ty; x: string; init: exp; pos: pos }
  | ChannelDecl of { channel: Ch.channel; level: L.level; potential: int; ty: T.ty; pos: pos }
  | InputDecl of { level: L.level; pos: pos }
and hl
  = Hl of { handler: string; level: L.level; potential: int; x: string; ty: T.ty; body: cmd; pos: pos }
and var = Var of { var_base: var_base; pos: pos}
and var_base
  = SimpleVar of string
  | SubscriptVar of {var: var; exp: exp}
and exp = Exp of { exp_base: exp_base; pos: pos }
and proj
  = Fst
  | Snd
and exp_base
  = IntExp of int
  | StringExp of string
  | VarExp of var
  | ProjExp of {proj: proj; exp:exp}
  | SizeExp of exp
  | OpExp of { left: exp; oper: oper; right: exp }
  | PairExp of (exp*exp)
  | ArrayExp of exp list
and cmd = Cmd of { cmd_base: cmd_base; pos: pos }
and cmd_base
  = SkipCmd
  | SeqCmd of { c1: cmd; c2: cmd }
  | AssignCmd of { var: var; exp: exp }
  | BindCmd of { var: var; exp: exp }
  | InputCmd of { var: var; size: exp }
  | SendCmd of { channel: Ch.channel; exp: exp }
  | IfCmd of { test: exp; thn: cmd; els: cmd }
  | WhileCmd of { test: exp; body: cmd }
  | OblivIfCmd of { test: exp; thn: cmd; els: cmd }
  | PrintCmd of { info: string option; exp: exp }
  | ExitCmd
 