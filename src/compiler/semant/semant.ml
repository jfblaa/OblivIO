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
    ; delta : (string, Ty.ty) H.t
    ; mutable input: Ty.ty option
    ; lambda : (string * string, Ty.ty) H.t
    ; hltable : (string, Ty.ty) H.t
    ; err :  Err.errorenv 
    }
;;

let _bot base =
  Ty.Type{base;level=L.bottom}

let raiseTo (T.Type{base;level}) lvl =
  T.Type{base;level=L.lub level lvl}

let errTy err pos msg =
  Err.error err pos @@ msg;
  _bot Ty.ERROR

let lookupVar ({gamma;delta;err;_}) v pos = 
  match H.find_opt delta v with
  | Some ty -> LOCAL,ty
  | None ->
    match H.find_opt gamma v with
    | Some ty -> STORE, ty
    | None -> LOCAL, errTy err pos @@ "undeclared variable " ^ v

let lookupInternal ({input;err;_}) pos = 
    match input with
    | Some ty -> ty
    | None -> errTy err pos @@ "undeclared internal channel"

let lookupHandler ({hltable;err;_}) hl pos = 
  match H.find_opt hltable hl with
  | Some ty -> ty
  | None -> errTy err pos @@ "undeclared handler " ^ hl

let lookupRemote ({lambda;err;_}) node channel pos = 
  match H.find_opt lambda (node,channel) with
  | Some ty -> ty
  | None -> errTy err pos @@ "undeclared channel " ^ node ^ "/" ^ channel

let e_ty (Exp{ty;_} as e) = (e,ty)
let e_ty_lvl (Exp{ty;_} as e) = (e,ty,Ty.level ty)
let v_ty (Var{ty;_} as v) = (v,ty)
let v_ty_loc (Var{ty;loc;_} as v) = (v,ty,loc)
let v_ty_lvl (Var{ty;_} as v) = (v,ty,T.level ty)

let v_ty_lvl_loc (Var{ty;loc;_} as v) = (v,ty,T.level ty,loc)

let rec isSameBase t1 t2 =
  match Ty.base t1, Ty.base t2 with
  | Ty.ERROR, _ -> true
  | _, Ty.ERROR -> true
  | T.INT, T.INT -> true
  | T.STRING, T.STRING -> true
  | T.PAIR (a1,a2), T.PAIR (b1,b2) ->
    isSameBase a1 b1 && isSameBase a2 b2
  | T.ARRAY t1, T.ARRAY t2 ->
    isSameBase t1 t2
  | _ -> false

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

let checkComparable t1 t2 err pos =
  let rec _check t1 t2 =
    match T.base t1, T.base t2 with
    | Ty.INT, Ty.INT -> ()
    | Ty.STRING, Ty.STRING -> ()
    | Ty.PAIR (a1,a2), Ty.PAIR (b1,b2) ->
      _check a1 b1; _check a2 b2
    | Ty.ARRAY t1, Ty.ARRAY t2 ->
      _check t1 t2
    | b1, b2 -> Err.error err pos @@ "types " ^ Ty.base_to_string b1 ^ " and " ^ Ty.base_to_string b2 ^ " do not match"
  in _check t1 t2

let rec checkAssignable t1 t2 err pos =
  checkFlowType t1 t2 err pos;
  match T.base t1, T.base t2 with
  | T.ERROR, _ -> ()
  | _, T.ERROR -> ()
  | T.INT, T.INT -> ()
  | T.STRING, T.STRING -> ()
  | T.PAIR (a1,a2), T.PAIR (b1,b2) ->
    checkAssignable a1 b1 err pos;
    checkAssignable a2 b2 err pos;
  | T.ARRAY t1, T.ARRAY t2 ->
    checkAssignable t1 t2 err pos
  | b1, b2 -> Err.error err pos @@ "cannot assign expression of type " ^ Ty.base_to_string b1 ^ " to variable of type " ^ Ty.base_to_string b2

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

exception NotImplemented of string

let rec transExp ({err;_} as ctxt) =
  let rec trexp (A.Exp{exp_base;pos}) =
    let (^!) exp_base ty = Exp{exp_base;ty;pos} in
    match exp_base with
    | IntExp n -> IntExp n ^! _bot Ty.INT
    | StringExp s -> StringExp s ^! _bot Ty.STRING
    | VarExp v ->
      let v,ty = v_ty @@ transVar ctxt v in
      VarExp v ^! ty
    | SizeExp e ->
      let e = trexp e in
      SizeExp e ^! _bot Ty.INT
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
    | ProjExp {proj;exp} -> 
      let (exp,ty,level) = e_ty_lvl @@ trexp exp in
      let proj,ty =
        match proj,T.base ty with
        | Fst, T.PAIR (t,_) -> Fst, raiseTo t level
        | Snd, T.PAIR (_,t) -> Snd, raiseTo t level
        | Fst, _ -> Fst, errTy err pos @@ "not a pair type " ^ T.to_string ty
        | Snd, _ -> Snd, errTy err pos @@ "not a pair type " ^ T.to_string ty in
      ProjExp{proj;exp} ^! ty
    | LengthExp{public;var} ->
      let var,ty,level = v_ty_lvl @@ transVar ctxt var in
      begin
      match T.base ty with
      | T.STRING | T.ARRAY _ ->
        if public
        then LengthExp{public;var} ^! T.Type{base=T.INT;level=L.bottom}
        else LengthExp{public;var} ^! T.Type{base=T.INT;level}
      | _ -> LengthExp{public;var} ^! errTy err pos @@ "not a string or array type " ^ T.to_string ty
      end
    | PairExp (a,b) ->
      let (a,aty) = e_ty @@ trexp a in
      let (b,bty) = e_ty @@ trexp b in
      let base = T.PAIR (aty,bty) in
      PairExp (a,b) ^! T.Type{base;level=L.bottom}
    | ArrayExp arr ->
      let ty = match arr with
      | hd::_ ->
        let _, ty = e_ty @@ transExp ctxt hd in
        ty
      | _ ->
        errTy err pos "array cannot be empty" in
      let f exp =
        let e,ety = e_ty @@ transExp ctxt exp in
        checkBaseType ty ety err pos;
        e in
      let arr = List.map f arr in
      let base = T.ARRAY ty in
      ArrayExp arr ^! T.Type{base;level=L.bottom}
      
  in trexp
and transVar ({err;_} as ctxt) =
  let rec trvar (A.Var{var_base;pos}) =
    let (^!) var_base ty = Var{var_base;loc=LOCAL;ty;pos} in
    let (^@) var_base ty = Var{var_base;loc=STORE;ty;pos} in
    match var_base with
    | SimpleVar x ->
      let varloc, ty = lookupVar ctxt x pos in
      begin
      match varloc with
        | LOCAL -> SimpleVar x ^! ty
        | STORE -> SimpleVar x ^@ ty
      end
    | SubscriptVar {var;exp} ->
      let var,vty,vlvl,loc = v_ty_lvl_loc @@ trvar var in
      let exp,ety,elvl = e_ty_lvl @@ transExp ctxt exp in
      checkInt ety err pos;
      let t = match T.base vty with
        | T.ARRAY t -> raiseTo t (L.lub vlvl elvl)
        | _ -> errTy err pos @@ "variable type not an array type: " ^ T.to_string vty in
      begin
      match loc with
      | LOCAL -> SubscriptVar{var;exp} ^! t
      | STORE -> SubscriptVar{var;exp} ^@ t
      end
  in trvar

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
      let var,varty,loc = v_ty_loc @@ transVar ctxt var in
      let e,ety = e_ty @@ transExp ctxt exp in
      begin
        match loc with
        | LOCAL -> ()
        | STORE -> 
          checkLowPC pc err pos
      end;
      checkAssignable ety varty err pos;
      fromBase @@ AssignCmd{var;exp=e}
    | BindCmd {var;exp} ->
      let var,varty = v_ty @@ transVar ctxt var in
      let e,ety = e_ty @@ transExp ctxt exp in
      checkAssignable (raiseTo ety pc) varty err pos;
      fromBase @@ BindCmd{var;exp=e}
    | InputCmd {var;size} ->
      let var,varty = v_ty @@ transVar ctxt var in
      let chty = lookupInternal ctxt pos in
      let size,sizety,sizelvl = e_ty_lvl @@ transExp ctxt size in
      checkAssignable (raiseTo chty pc) varty err pos;
      checkInt sizety err pos;
      checkFlow sizelvl L.bottom err pos;
      fromBase @@ InputCmd{var;size}
    | SendCmd {node;channel;exp} ->
      let chty = lookupRemote ctxt node channel pos in
      let e,ety = e_ty @@ transExp ctxt exp in
      checkAssignable (raiseTo ety pc) chty err pos;
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

let transLocal ({delta;err;_} as ctxt) (A.LocalDecl {ty_opt;x;init;pos}) =
  let init,initty = e_ty @@ transExp ctxt init in
  begin
  match ty_opt with
  | Some ty ->
    H.add delta x ty;
    checkAssignable initty ty err pos;
  | None ->
    H.add delta x initty;
  end;
  LocalDecl{x;ty_opt;init;pos}

let transDecl ({gamma;input;lambda;err;_} as ctxt) dec =
  match dec with
  | A.VarDecl {ty_opt;x;init;pos} ->
    if H.mem gamma x
    then Err.error err pos @@ "variable " ^ x ^ " already declared";
    let init,initty = e_ty @@ transExp ctxt init in
    begin
    match ty_opt with
    | Some ty ->
      H.add gamma x ty;
      checkAssignable initty ty err pos;
    | None -> H.add gamma x initty
    end;
    VarDecl{x;ty_opt;init;pos}
  | A.ChannelDecl {ty;node;ch;pos} ->
    H.add lambda (node,ch) ty;
    ChannelDecl{node;ch;ty;pos}
  | A.InputDecl{ty;pos} ->
    begin
    match input with
    | Some _ -> Err.error err pos @@ "input channel already declared"
    | None -> ctxt.input <- Some ty
    end;
    checkString ty err pos;
    InputDecl{ty;pos}

let transCh ({delta;_} as ctxt) (A.Ch{ch;sender_opt;x;ty;decls;prelude;body;pos}) =
  H.add delta x ty;
  begin
    match sender_opt with
    | Some sender ->
      let base = T.STRING in
      let level = L.bottom in
      H. add delta sender (T.Type{base;level})
    | None -> ()
  end;
  let decls = List.map (transLocal ctxt) decls in
  let prelude = Option.map (transCmd ctxt L.bottom) prelude in
  let body = transCmd ctxt (T.level ty) body in
  H.clear delta;
  Ch{ch;sender_opt;x;ty;decls;prelude;body;pos}

let transHlDecl {hltable;_} (A.Ch{ch;ty;_}) =
  H.add hltable ch ty

let transProg (A.Prog{node;decls;chs}) =
  let ctxt = 
    { gamma = H.create 1024
    ; delta = H.create 1024
    ; input = None
    ; lambda = H.create 1024
    ; hltable = H.create 1024
    ; err = Err.initial_env
    } in
  let decls = List.map (transDecl ctxt) decls in
  List.iter (transHlDecl ctxt) chs;
  let chs = List.map (transCh ctxt) chs in
  not (Err.any_errors ctxt.err), Prog{node;decls;chs}
