(**************************************************************************)
(* AU Compilation. This file needs no modifications                       *)
(**************************************************************************)

type pos = Lexing.position 
type level = Level.level
type value = Value.value
include Oper

(* Note the use of Ocaml inline records in our AST declaration 
  https://caml.inria.fr/pub/docs/manual-ocaml/manual040.html *)

type program = Prog of { level: level; vardecls: vardecl list; hns: hn list }
and vardecl
  = VarDecl of { var: var; value: value; level: level; pos: pos }
  (*| InDecl of { level: level; messages: (int*value option) list; pos: pos }*)
and hn
  = Hn of { level: level; header: string; svar: var * var; cmd: cmd; pos: pos }
  | OblivHn of { level: level; header: string; svar: var * var; cmd: cmd; pos: pos }
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
  | SeqCmd of {c1: cmd; c2: cmd}
  | SkipCmd
  | SleepCmd of exp
  | IfCmd of { test: exp; thn: cmd; els: cmd }
  | WhileCmd of { test: exp; body: cmd }
  | SendCmd of { level: level; header: string; exp: exp }
  | MimicCmd of cmd
