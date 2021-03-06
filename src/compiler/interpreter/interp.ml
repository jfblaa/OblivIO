open Common

module A = Absyn
module O = Oper
module L = Level
module M = Strmap
module V = Value
module N = Network

type handler_map = (L.level * A.cmd) M.strmap

(* TODO: Rethink this! Need to bind msg variable in producer state only! *)
type _config
  = { handlers: handler_map
    ; active_hn: (A.cmd * (V.value M.strmap)) option
    ; mem: V.value M.strmap
    ; msg_queue: N.message list
    }

type sys_config
  = { nodes: prog_config list
    ; mutable trace: N.message list
    }

exception InterpFatal
exception NotImplemented of string

let op oper (v1,s1) (v2,s2) =
  match oper,v1,v2 with
  | O.EqOp,V.IntVal a,V.IntVal b ->
    V.IntVal(if a=b then 1 else 0), 1
  | O.NeqOp,V.IntVal a,V.IntVal b ->
    V.IntVal(if a<>b then 1 else 0), 1
  | O.LtOp,V.IntVal a,V.IntVal b ->
    V.IntVal(if a<b then 1 else 0), 1
  | O.LeOp,V.IntVal a,V.IntVal b ->
    V.IntVal(if a<=b then 1 else 0), 1
  | O.GtOp,V.IntVal a,V.IntVal b ->
    V.IntVal(if a>b then 1 else 0), 1
  | O.GeOp,V.IntVal a,V.IntVal b ->
    V.IntVal(if a>=b then 1 else 0), 1
  | O.AndOp,V.IntVal a,V.IntVal b ->
    V.IntVal(if a<>0&&b<>0 then 1 else 0), 1
  | O.OrOp,V.IntVal a,V.IntVal b ->
    V.IntVal(if a<>0||b<>0 then 1 else 0), 1
  | O.PlusOp,V.IntVal a,V.IntVal b ->
    V.IntVal(a+b), max s1 s2
  | O.MinusOp,V.IntVal a,V.IntVal b ->
    V.IntVal(a-b), max s1 s2
  | O.EqOp,V.StringVal a,V.StringVal b ->
    V.IntVal(if String.equal a b then 1 else 0), max s1 s2
  | O.NeqOp,V.StringVal a,V.StringVal b ->
    IntVal(if String.equal a b then 0 else 1), max s1 s2
  | O.ConcatOp,V.StringVal a,V.StringVal b ->
    V.StringVal(a^b), s1+s2
  | _ -> raise InterpFatal
let (^?) mem x =
  match M.look(mem,x) with
  | Some v -> v
  | None -> raise InterpFatal

let _int v =
  match v with
  | V.IntVal i -> i
  | _ -> raise InterpFatal

let _string v =
  match v with
  | V.StringVal s -> s
  | _ -> raise InterpFatal

let eval {mem;_} =
  let rec _E (A.Exp{exp_base;_}) =
    match exp_base with
    | A.IntExp i -> (V.IntVal i, 8)
    | A.StringExp s -> (V.StringVal s, 8 * String.length s)
    | A.VarExp x -> mem ^? x
    | A.OpExp {left;oper;right} ->
      let lval = _E left in
      let rval = _E right in
      op oper lval rval
  in _E

let producer_step (ctxt: prog_config) cmd  =
  let rec _P (A.Cmd{cmd_base;pos}) =
  match cmd_base with
  | _ -> raise @@ NotImplemented ""
  in _P cmd


let consumer_step (ctxt: prog_config) =
  raise @@ NotImplemented ""

let step ({cmd_opt;_} as ctxt: prog_config) =
  match cmd_opt with
  | Some cmd -> producer_step ctxt cmd
  | None -> consumer_step ctxt 