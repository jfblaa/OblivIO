open Common
open Tabsyn
open Oper

module A = Absyn
module Err = Errorenv
module Ty = Types
module L = Level

module H = Hashtbl

(* gamma = H.create 1024 *)

type context 
  = { gamma : (string, Ty.ty) H.t
    ; lambda : (string, Ty.ty) H.t
    ; err :  Err.errorenv 
    }
;;

let _bot base =
  Ty.Type{base;level=L.bottom}

let errTy err pos msg =
  Err.error err pos @@ msg;
  _bot Ty.ERROR

let lookupVar ({gamma;err;_}) v pos = 
  match H.find_opt gamma v with
  | Some ty -> ty
  | None -> errTy err pos @@ "undeclared variable " ^ v

let lookupCh ({lambda;err;_}) ch pos = 
  match H.find_opt lambda ch with
  | Some ty -> ty
  | None -> errTy err pos @@ "undeclared channel " ^ ch

let e_ty (Exp{ty;_} as e) = (e,ty)
let e_ty_lvl (Exp{ty;_} as e) = (e,ty,Ty.level ty)

let isEqual t1 t2 =
  match Ty.base t1, Ty.base t2 with
  | Ty.ERROR, _ -> true
  | _, Ty.ERROR -> true
  | a, b -> a = b

let checkEqual t1 t2 err pos =
  if not (isEqual t1 t2)
  then Err.error err pos @@ "type " ^ (Ty.base_to_string @@ Ty.base t1) ^ " not equal to type " ^ (Ty.base_to_string @@ Ty.base t2)

let checkFlow l1 l2 err pos =
  if not (L.flows_to l1 l2)
  then Err.error err pos @@ "illegal flow from " ^ L.to_string l1 ^ " to " ^ L.to_string l2

let checkFlowType t1 t2 err pos =
  let l1, l2 = Ty.level t1, Ty.level t2 in
  if not (L.flows_to l1 l2)
  then Err.error err pos @@ "illegal flow from " ^ L.to_string l1 ^ " to " ^ L.to_string l2

let checkComparable t1 t2 err pos =
  match Ty.base t1, Ty.base t2 with
  | Ty.INT, Ty.INT -> ()
  | Ty.STRING, Ty.STRING -> ()
  | b1, b2 -> Err.error err pos @@ "types " ^ Ty.base_to_string b1 ^ " and " ^ Ty.base_to_string b2 ^ " do not match"

let checkLowPc pc v err pos =
  if not (L.flows_to pc L.bottom)
  then Err.error err pos @@ "assignment to non-obliv variable " ^ v ^ " only allowed under low pc" 

let isNonObliv t =
  match Ty.base t with
  | (Ty.OBLIV _) -> false
  | _ -> true

let checkObliv t err pos =
  match Ty.base t with
  | (Ty.OBLIV _) -> ()
  | b -> Err.error err pos @@ "obliv type required, " ^ Ty.base_to_string b ^ " provided"

let checkNonObliv t err pos =
  match Ty.base t with
  | (Ty.OBLIV _) as b -> Err.error err pos @@ "non-obliv type required, " ^ Ty.base_to_string b ^ " provided"
  | _ -> ()

let toObliv t err pos =
  checkNonObliv t err pos;
  let level = Ty.level t in
  let base = Ty.OBLIV (Ty.base t) in
  Ty.Type{base;level}

let fromObliv t err pos =
  checkObliv t err pos;
  let level = Ty.level t in
  let base =
    match Ty.base t with
    | Ty.OBLIV t -> t
    | _ -> Ty.ERROR in
  Ty.Type{base;level}

let checkInt t err pos =
  match Ty.base t with
  | Ty.INT -> ()
  | b -> Err.error err pos @@ "int required, " ^ Ty.base_to_string b ^ " provided"

let checkString t err pos =
  match Ty.base t with
  | Ty.STRING -> ()
  | b -> Err.error err pos @@ "string required, " ^ Ty.base_to_string b ^ " provided"

exception NotImplemented

let transExp ({err;_} as ctxt) =
  let rec trexp (A.Exp{exp_base;pos}) =
    let (^!) exp_base ty = Exp{exp_base;ty;pos} in
    match exp_base with
    | IntExp n -> IntExp n ^! _bot Ty.INT
    | StringExp s -> StringExp s ^! _bot Ty.STRING
    | VarExp v ->
      let ty = lookupVar ctxt v pos in
      VarExp (v, ty) ^! ty
    | ProjExp e ->
      let (e,ety) = e_ty @@ trexp e in
      ProjExp e ^! fromObliv ety err pos
    | InjExp e ->
      let (e,ety) = e_ty @@ trexp e in
      InjExp e ^! toObliv ety err pos
    | SizeExp e ->
      let e,ety = e_ty @@ trexp e in
      checkNonObliv ety err pos;
      SizeExp e ^! _bot Ty.INT
    | OpExp {left;oper;right} ->
      let (left,lty) = e_ty @@ trexp left in
      let (right,rty) = e_ty @@ trexp right in
      let level = L.lub (Ty.level lty) (Ty.level rty) in
      let base =
        match oper with
        | PlusOp | MinusOp | LtOp | LeOp | GtOp | GeOp | AndOp |OrOp ->
          checkInt lty err pos;
          checkInt rty err pos;
          Ty.INT
        | EqOp | NeqOp ->
          checkComparable lty rty err pos;
          Ty.INT
        | ConcatOp ->
          checkString lty err pos;
          checkString rty err pos;
          Ty.STRING in
      OpExp{left;oper;right} ^! Ty.Type{base;level}
  in trexp

let transCmd ({err;_} as ctxt) =
  let rec trcmd pc (A.Cmd{cmd_base;pos}) =
    let fromBase cmd_base = Cmd{cmd_base;pos} in
    match cmd_base with
    | SkipCmd -> fromBase SkipCmd
    | SeqCmd {c1;c2} ->
      let c1 = trcmd pc c1 in
      let c2 = trcmd pc c2 in
      fromBase @@ SeqCmd {c1;c2}
    | AssignCmd {var;exp} ->
      let varty = lookupVar ctxt var pos in
      let e,ety = e_ty @@ transExp ctxt exp in
      if isNonObliv varty
      then checkLowPc pc var err pos;
      checkEqual ety varty err pos;
      checkFlowType ety varty err pos;
      fromBase @@ AssignCmd{var=(var,varty);exp=e}
    | OblivAssignCmd {var;exp} ->
      let varty = lookupVar ctxt var pos in
      let e,ety = e_ty @@ transExp ctxt exp in
      checkNonObliv varty err pos;
      let ety' = fromObliv ety err pos in
      checkEqual ety' varty err pos;
      checkFlowType ety' varty err pos;
      fromBase @@ OblivAssignCmd{var=(var,varty);exp=e}
    | OutputCmd {channel;exp} ->
      let chty = lookupCh ctxt channel pos in
      let e,ety = e_ty @@ transExp ctxt exp in
      checkNonObliv ety err pos;
      checkEqual ety chty err pos;
      checkFlowType ety chty err pos;
      fromBase @@ OutputCmd{channel;exp=e}
    | IfCmd{test;thn;els} ->
      let test,testty,testlvl = e_ty_lvl @@ transExp ctxt test in
      checkInt testty err pos;
      checkFlow testlvl L.bottom err pos;
      let thn = trcmd pc thn in
      let els = trcmd pc els in
      fromBase @@ IfCmd{test;thn;els}
    | OblivIfCmd{test;thn;els} ->
      let test,testty,testlvl = e_ty_lvl @@ transExp ctxt test in
      checkInt testty err pos;
      let thn = trcmd (L.lub pc testlvl) thn in
      let els = trcmd (L.lub pc testlvl) els in
      fromBase @@ OblivIfCmd{test;thn;els}
    | WhileCmd{test;body} ->
      let test,testty,testlvl = e_ty_lvl @@ transExp ctxt test in
      checkInt testty err pos;
      checkFlow testlvl L.bottom err pos;
      let body = trcmd pc body in
      fromBase @@ WhileCmd{test;body}
    | PhantomCmd c ->
      fromBase @@ PhantomCmd (trcmd pc c)
    | PrintCmd {info;exp} ->
      let exp,ety = e_ty @@transExp ctxt exp in
      checkNonObliv ety err pos;
      fromBase @@ PrintCmd{info;exp}
  in trcmd

let transTy ty =
  let (^!) base level = Ty.Type{base;level} in
  match ty with
  | A.IntType level -> Ty.INT ^! level
  | A.StringType level -> Ty.STRING ^! level
  | A.OblivIntType level -> (Ty.OBLIV Ty.INT) ^! level
  | A.OblivStringType level -> (Ty.OBLIV Ty.STRING) ^! level

let transCh ({err;_} as ctxt) (A.Ch{ty;name;var;prelude;body;pos}) =
  let ty = transTy ty in
  let oblivty = toObliv ty err pos in
  let varty = lookupVar ctxt var pos in
  checkEqual oblivty varty err pos;
  checkFlowType oblivty varty err pos; checkFlowType varty oblivty err pos;
  let prelude = transCmd ctxt L.bottom prelude in
  let body = transCmd ctxt (Ty.level ty) body in
  Ch{ty;name;var=(var,varty);prelude;body;pos}

let transDecl ({gamma;lambda;err} as ctxt) dec =
  match dec with
  | A.VarDecl {ty;var;init;pos} ->
    let ty = transTy ty in
    H.add gamma var ty;
    let init,initty = e_ty @@ transExp ctxt init in
    checkEqual initty ty err pos;
    checkFlowType initty ty err pos;
    VarDecl{var=(var,ty);init;pos}
  | A.ChDecl {ty;name;pos} ->
    let ty = transTy ty in
    H.add lambda name ty;
    ChDecl{name;ty;pos}

let transProg (A.Prog{node;adv;decls;init;chs}) =
  let ctxt = 
    { gamma = H.create 1024
    ; lambda = H.create 1024
    ; err = Err.initial_env
    } in
  let enter (A.Ch{ty;name;_}) =
    H.add ctxt.lambda name (transTy ty) in
  List.iter enter chs;
  let decls = List.map (transDecl ctxt) decls in
  let init = Option.map (transCmd ctxt L.bottom) init in
  let chs = List.map (transCh ctxt) chs in
  not (Err.any_errors ctxt.err), Prog{node;adv;decls;init;chs}
