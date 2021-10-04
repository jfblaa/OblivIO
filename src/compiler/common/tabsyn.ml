type pos = Lexing.position 
type level = Level.level

module T=Types

include Oper

type program = Prog of { node: string; decls: decl list; chs: ch list }
and decl
  = VarDecl of { x: string; ty: T.ty; init: exp; pos: pos }
  | ChannelDecl of { node: string; ch: string; ty: T.ty; pos: pos }
  | InputDecl of {ty: T.ty; pos: pos }
and hldecl = LocalDecl of { x: string; ty: T.ty; init: exp; pos: pos }
and ch
  = Ch of { ty: T.ty; ch: string; x: string; y: string; decls: hldecl list; prelude: cmd option; body: cmd; pos: pos }
and var
  = LocalVar of { var_base: var_base; ty: T.ty; pos: pos }
  | StoreVar of { var_base: var_base; ty: T.ty; pos: pos }
and var_base =
  | SimpleVar of string
  | SubscriptVar of { var: var; exp: exp }
and exp = Exp of { exp_base: exp_base; ty: T.ty; pos: pos }
and proj
  = Fst
  | Snd
and exp_base
  = IntExp of int
  | StringExp of string
  | VarExp of var
  | ProjExp of {proj: proj; exp: exp}
  | SizeExp of exp
  | OpExp of { left: exp; oper: oper; right: exp }
  | PairExp of (exp*exp)
  | ArrayExp of exp list
  | ErrorExp
and cmd = Cmd of { cmd_base: cmd_base; pos: pos }
and cmd_base
  = SkipCmd
  | SeqCmd of { c1: cmd; c2: cmd }
  | AssignCmd of { var: var; exp: exp }
  | BindCmd of { var: var; exp: exp }
  | InputCmd of { var: var; size: exp }
  | SendCmd of { node: string; channel: string; exp: exp }
  | IfCmd of { test: exp; thn: cmd; els: cmd }
  | WhileCmd of { test: exp; body: cmd }
  | OblivIfCmd of { test: exp; thn: cmd; els: cmd }
  | PopCmd
  | PrintCmd of { info: string option; exp: exp }
