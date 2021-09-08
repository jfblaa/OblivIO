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
    ; mutable input: Ty.ty option
    ; lambda : (string * string, Ty.ty) H.t
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

let lookupInternal ({input;err;_}) pos = 
    match input with
    | Some ty -> ty
    | None -> errTy err pos @@ "undeclared internal channel"

let lookupRemote ({lambda;err;_}) node ch pos = 
  match H.find_opt lambda (node,ch) with
  | Some ty -> ty
  | None -> errTy err pos @@ "undeclared remote channel " ^ ch ^ " at node " ^ node

let e_ty (Exp{ty;_} as e) = (e,ty)
let e_ty_lvl (Exp{ty;_} as e) = (e,ty,Ty.level ty)

let isSameBase t1 t2 =
  match Ty.base t1, Ty.base t2 with
  | Ty.ERROR, _ -> true
  | _, Ty.ERROR -> true
  | a, b -> a = b

let checkBaseType t1 t2 err pos =
  if not (isSameBase t1 t2)
  then Err.error err pos @@ "type " ^ (Ty.base_to_string @@ Ty.base t1) ^ " not equal to type " ^ (Ty.base_to_string @@ Ty.base t2)

let checkFlow l1 l2 err pos =
  if not (L.flows_to l1 l2)
  then Err.error err pos @@ "illegal flow from " ^ L.to_string l1 ^ " to " ^ L.to_string l2

let checkFlowType t1 t2 err pos =
  let l1, l2 = Ty.level t1, Ty.level t2 in
  if not (L.flows_to l1 l2)
  then Err.error err pos @@ "illegal flow from " ^ L.to_string l1 ^ " to " ^ L.to_string l2

let checkFlowTypePC pc t1 t2 err pos =
    let l1, l2 = Ty.level t1, Ty.level t2 in
    if not (L.flows_to (L.lub pc l1) l2)
    then Err.error err pos @@ "illegal flow from " ^ L.to_string l1 ^ " to " ^ L.to_string l2 ^ " at pc " ^ L.to_string pc

let checkComparable t1 t2 err pos =
  match Ty.base t1, Ty.base t2 with
  | Ty.INT, Ty.INT -> ()
  | Ty.STRING, Ty.STRING -> ()
  | b1, b2 -> Err.error err pos @@ "types " ^ Ty.base_to_string b1 ^ " and " ^ Ty.base_to_string b2 ^ " do not match"

let checkLowPC pc err pos =
  if not (L.flows_to pc L.bottom)
  then Err.error err pos @@ "pc must be low"

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
    | SizeExp e ->
      let e = trexp e in
      SizeExp e ^! _bot Ty.INT
    | QuestionExp e ->
      let (e,ety) = e_ty @@ trexp e in
      QuestionExp e ^! Ty.Type{base=Ty.INT; level=Ty.level ety}
    | OpExp {left;oper;right} ->
      let (left,lty) = e_ty @@ trexp left in
      let (right,rty) = e_ty @@ trexp right in
      let level = L.lub (Ty.level lty) (Ty.level rty) in
      let base =
        match oper with
        | PlusOp | MinusOp | TimesOp | DivideOp | LtOp | LeOp | GtOp | GeOp | AndOp |OrOp ->
          checkInt lty err pos;
          checkInt rty err pos;
          Ty.INT
        | EqOp | NeqOp ->
          checkComparable lty rty err pos;
          Ty.INT 
        | CaretOp -> 
          checkString lty err pos;
          checkString rty err pos;
          Ty.STRING 
        | PadOp ->
          checkString lty err pos;
          checkInt rty err pos;
          Ty.STRING
        in
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
      checkLowPC pc err pos;
      checkBaseType ety varty err pos;
      checkFlowType ety varty err pos;
      fromBase @@ AssignCmd{var=(var,varty);exp=e}
    | BindCmd {var;exp} ->
      let varty = lookupVar ctxt var pos in
      let e,ety = e_ty @@ transExp ctxt exp in
      checkBaseType ety varty err pos;
      checkFlowTypePC pc ety varty err pos;
      fromBase @@ BindCmd{var=(var,varty);exp=e}
    | InputCmd {var;default} ->
      let varty = lookupVar ctxt var pos in
      let chty = lookupInternal ctxt pos in
      let default,defaultty = e_ty @@ transExp ctxt default in
      checkBaseType chty varty err pos;
      checkFlowTypePC pc chty varty err pos;
      checkBaseType defaultty varty err pos;
      checkFlowTypePC pc defaultty varty err pos;
      fromBase @@ InputCmd{var=(var,varty);default}
    | SendCmd {node;channel;exp} ->
      let chty = lookupRemote ctxt node channel pos in
      let e,ety = e_ty @@ transExp ctxt exp in
      checkBaseType ety chty err pos;
      checkFlowTypePC pc ety chty err pos;
      fromBase @@ SendCmd{node;channel;exp=e}
    | IfCmd{test;thn;els} ->
      let test,testty,testlvl = e_ty_lvl @@ transExp ctxt test in
      checkInt testty err pos;
      checkFlow testlvl L.bottom err pos;
      let thn = trcmd pc thn in
      let els = trcmd pc els in
      fromBase @@ IfCmd{test;thn;els}
    | WhileCmd{test;body} ->
      let test,testty,testlvl = e_ty_lvl @@ transExp ctxt test in
      checkInt testty err pos;
      checkFlow testlvl L.bottom err pos;
      let body = trcmd pc body in
      fromBase @@ WhileCmd{test;body}
    | OblivIfCmd{test;thn;els} ->
      let test,testty,testlvl = e_ty_lvl @@ transExp ctxt test in
      checkInt testty err pos;
      let thn = trcmd (L.lub pc testlvl) thn in
      let els = trcmd (L.lub pc testlvl) els in
      fromBase @@ OblivIfCmd{test;thn;els}
    | PrintCmd {info;exp} ->
      let exp = transExp ctxt exp in
      fromBase @@ PrintCmd{info;exp}
  in trcmd

let transTy ty =
  let (^!) base level = Ty.Type{base;level} in
  match ty with
  | A.IntType level -> Ty.INT ^! level
  | A.StringType level -> Ty.STRING ^! level

let transCh ({gamma;err;_} as ctxt) (A.Ch{ty;ch;var;body;pos}) =
  let ty = transTy ty in
  if H.mem gamma var
  then Err.error err pos @@ "variable " ^ var ^ " already declared";
  H.add gamma var ty;
  let body = transCmd ctxt L.bottom body in
  H.remove gamma var;
  Ch{ty;ch;var=(var,ty);body;pos}

let transDecl ({gamma;input;lambda;err} as ctxt) dec =
  match dec with
  | A.VarDecl {ty;var;init;pos} ->
    let ty = transTy ty in
    if H.mem gamma var
    then Err.error err pos @@ "variable " ^ var ^ " already declared";
    H.add gamma var ty;
    let init,initty = e_ty @@ transExp ctxt init in
    checkBaseType initty ty err pos;
    checkFlowType initty ty err pos;
    VarDecl{var=(var,ty);init;pos}
  | A.ChannelDecl {ty;node;ch;pos} ->
    let ty = transTy ty in
    H.add lambda (node,ch) ty;
    ChannelDecl{node;ch;ty;pos}
  | A.InputDecl{ty;pos} ->
    let ty = transTy ty in
    begin
    match input with
    | Some _ -> Err.error err pos @@ "input channel already declared"
    | None -> ctxt.input <- Some ty
    end;
    checkString ty err pos;
    InputDecl{ty;pos}

let transProg (A.Prog{node;decls;chs}) =
  let ctxt = 
    { gamma = H.create 1024
    ; input = None
    ; lambda = H.create 1024
    ; err = Err.initial_env
    } in
  (*let enter (A.Ch{ty;node;ch;_}) =
    H.add ctxt.lambda (node,ch) (transTy ty) in
  List.iter enter chs;*)
  let decls = List.map (transDecl ctxt) decls in
  let chs = List.map (transCh ctxt) chs in
  not (Err.any_errors ctxt.err), Prog{node;decls;chs}
