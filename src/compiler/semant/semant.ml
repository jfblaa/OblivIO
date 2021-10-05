open Common
open Tabsyn
open Oper

module A = Absyn
module Err = Errorenv
module Ty = Types
module L = Level
module C = Channel

module H = Hashtbl

(* gamma = H.create 1024 *)

type context 
  = { gamma : (string, Ty.ty) H.t
    ; delta : (string, Ty.ty) H.t
    ; mutable input: Ty.ty option
    ; lambda : (C.channel, Ty.chty) H.t
    ; hltable : (string, Ty.ty) H.t
    ; err :  Err.errorenv 
    }
;;

let _bot base =
  Ty.Type{base;level=L.bottom}

let errTy err pos msg =
  Err.error err pos @@ msg;
  _bot Ty.ERROR

let errChTy err pos msg =
  Err.error err pos @@ msg;
  let reads = _bot Ty.ERROR in
  let writes = None in
  T.ChType{reads;writes}

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

let lookupRemote ({lambda;err;_}) channel pos = 
  match H.find_opt lambda channel with
  | Some ty -> ty
  | None -> errChTy err pos @@ "undeclared channel " ^ C.to_string channel

let e_ty (Exp{ty;_} as e) = (e,ty)
let e_ty_lvl (Exp{ty;_} as e) = (e,ty,Ty.level ty)
let v_ty (Var{ty;_} as v) = (v,ty)
let v_ty_loc (Var{ty;loc;_} as v) = (v,ty,loc)
let v_ty_lvl (Var{ty;_} as v) = (v,ty,T.level ty)

let v_ty_lvl_loc (Var{ty;loc;_} as v) = (v,ty,T.level ty,loc)

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
        | Fst, T.PAIR (base,_) -> Fst, T.Type{base;level}
        | Snd, T.PAIR (_,base) -> Snd, T.Type{base;level}
        | Fst, _ -> Fst, errTy err pos @@ "not a pair type " ^ T.to_string ty
        | Snd, _ -> Snd, errTy err pos @@ "not a pair type " ^ T.to_string ty in
      ProjExp{proj;exp} ^! ty
    | LengthExp{public;var} ->
      let var,_,level = v_ty_lvl @@ transVar ctxt var in
      if public
      then LengthExp{public;var} ^! T.Type{base=T.INT;level=L.bottom}
      else LengthExp{public;var} ^! T.Type{base=T.INT;level}
    | PairExp (a,b) ->
      let (a,aty,alvl) = e_ty_lvl @@ trexp a in
      let (b,bty,blvl) = e_ty_lvl @@ trexp b in
      let base = T.PAIR (T.base aty,T.base bty) in
      let level = L.lub alvl blvl in
      PairExp (a,b) ^! T.Type{base;level}
    | ArrayExp arr ->
      let ty,lvl = match arr with
      | hd::_ ->
        let _, ty,lvl = e_ty_lvl @@ transExp ctxt hd in
        ty, lvl
      | _ ->
        errTy err pos "array cannot be empty", L.bottom in
      let f lvl exp =
        let e,ety,elvl = e_ty_lvl @@ transExp ctxt exp in
        checkBaseType ty ety err pos;
        L.lub lvl elvl, e in
      let level, arr = List.fold_left_map f lvl arr in
      let base = T.ARRAY (T.base ty) in
      ArrayExp arr ^! T.Type{base;level}
      
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
      let exp,ety = e_ty @@ transExp ctxt exp in
      checkInt ety err pos;
      checkFlowType ety vty err pos; 
      let t = match T.base vty with
        | T.ARRAY base -> T.Type{base;level=vlvl}
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
      checkBaseType ety varty err pos;
      checkFlowType ety varty err pos;
      fromBase @@ AssignCmd{var;exp=e}
    | BindCmd {var;exp} ->
      let var,varty = v_ty @@ transVar ctxt var in
      let e,ety = e_ty @@ transExp ctxt exp in
      checkBaseType ety varty err pos;
      checkFlowTypePC pc ety varty err pos;
      fromBase @@ BindCmd{var;exp=e}
    | InputCmd {var;size} ->
      let var,varty = v_ty @@ transVar ctxt var in
      let chty = lookupInternal ctxt pos in
      let size,sizety,sizelvl = e_ty_lvl @@ transExp ctxt size in
      checkBaseType chty varty err pos;
      checkFlowType chty varty err pos;
      checkInt sizety err pos;
      checkFlow sizelvl L.bottom err pos;
      fromBase @@ InputCmd{var;size}
    | SendCmd {channel;replyto;exp} ->
      let T.ChType{reads;writes} = lookupRemote ctxt channel pos in
      let e,ety = e_ty @@ transExp ctxt exp in
      checkBaseType ety reads err pos;
      checkFlowTypePC pc ety reads err pos;
      begin 
        match writes,replyto with
        | Some wty,Some hl ->
          let hlty = lookupHandler ctxt hl pos in
          checkBaseType wty hlty err pos;
          checkFlowType wty hlty err pos
        | None, None -> ()
        | Some wty, None ->
          Err.error err pos @@ "unhandled reply of type " ^ T.to_string wty ^ " from channel " ^ C.to_string channel
        | None, Some _ ->
          Err.error err pos @@ "channel " ^ (C.to_string channel) ^ " does not reply"
      end;
      fromBase @@ SendCmd{channel;replyto;exp=e}
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

let transLocal ({delta;err;_} as ctxt) (A.LocalDecl {ty;x;init;pos}) =
  H.add delta x ty;
  let init,initty = e_ty @@ transExp ctxt init in
  checkBaseType initty ty err pos;
  checkFlowType initty ty err pos;
  LocalDecl{x;ty;init;pos}

let transDecl ({gamma;input;lambda;err;_} as ctxt) dec =
  match dec with
  | A.VarDecl {ty;x;init;pos} ->
    if H.mem gamma x
    then Err.error err pos @@ "variable " ^ x ^ " already declared";
    H.add gamma x ty;
    let init,initty = e_ty @@ transExp ctxt init in
    checkBaseType initty ty err pos;
    checkFlowType initty ty err pos;
    VarDecl{x;ty;init;pos}
  | A.ChannelDecl {chty;node;ch;pos} ->
    H.add lambda (Explicit (node,ch)) chty;
    ChannelDecl{node;ch;chty;pos}
  | A.InputDecl{ty;pos} ->
    begin
    match input with
    | Some _ -> Err.error err pos @@ "input channel already declared"
    | None -> ctxt.input <- Some ty
    end;
    checkString ty err pos;
    InputDecl{ty;pos}

let transCh ({delta;lambda;_} as ctxt) (A.Ch{ch;x;ty;replych;decls;prelude;body;pos}) =
  H.add delta x ty;
  begin 
    match replych with
    | Some (ch,chty) -> H.add lambda (Implicit ch) chty
    | None -> ()
  end;
  let decls = List.map (transLocal ctxt) decls in
  let prelude = Option.map (transCmd ctxt L.bottom) prelude in
  let body = transCmd ctxt (T.level ty) body in
  H.clear delta;
  begin 
    match replych with
    | Some (ch,_) -> H.remove lambda (Implicit ch);
    | None -> ()
  end;
  Ch{ch;x;ty;replych;decls;prelude;body;pos}

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
