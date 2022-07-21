open Common
open Tabsyn
open Oper

module A = Absyn
module Err = Errorenv
module Ty = Types
module L = Level
module Ch = Channel

module H = Hashtbl

module ST = Set.Make(Channel)

type context 
  = { gamma: (string, Ty.ty) H.t
    ; lambda: (Ch.channel, (L.level * Ty.ty * ST.t * int*int)) H.t
    ; delta: (string, Ty.ty) H.t
    ; mutable input: L.level option
    ; err:  Err.errorenv 
    }
;;

let _bot base =
  Ty.Type{base;level=L.bottom}

let raiseTo (T.Type{base;level}) lvl =
  T.Type{base;level=L.lub level lvl}

let errLvl err pos msg =
  Err.error err pos @@ msg;
  L.bottom

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
    | Some level -> level
    | None -> errLvl err pos @@ "undeclared internal channel"

let lookupRemote ({lambda;err;_}) ch pos = 
  match H.find_opt lambda ch with
  | Some lvl_ty_cap -> lvl_ty_cap
  | None -> L.bottom, errTy err pos @@ "undeclared channel " ^ Ch.to_string ch, ST.empty, 0, 0

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

let rec varname (Var{var_base;_}) =
  match var_base with
  | SimpleVar x -> x
  | SubscriptVar{var;_} -> varname var

let transCmd ({err;_} as ctxt) hlchannel declared_reach =
  let rec trcmd pc (A.Cmd{cmd_base;pos}): cmd * ST.t * int * int =
    let fromBase cmd_base = Cmd{cmd_base;pos} in
    match cmd_base with
    | SkipCmd -> fromBase SkipCmd, ST.empty, 0, 0
    | SeqCmd {c1;c2} ->
      let (c1,r1,co1,o1) = trcmd pc c1 in
      let (c2,r2,co2,o2) = trcmd pc c2 in
      fromBase @@ SeqCmd {c1;c2}, ST.union r1 r2, co1 + co2, o1 + o2
    | AssignCmd {var;exp} ->
      let var,varty,varloc = v_ty_loc @@ transVar ctxt var in
      let e,ety = e_ty @@ transExp ctxt exp in
      checkLowPC pc err pos;
      begin
        match varloc with
        | LOCAL ->
          Err.error err pos @@ "handler variable " ^ varname var ^ " is read-only";
        | STORE -> 
          checkLowPC pc err pos
      end;
      checkAssignable ety varty err pos;
      fromBase @@ AssignCmd{var;exp=e}, ST.empty, 0, 0
    | BindCmd {var;exp} ->
      let var,varty,varloc = v_ty_loc @@ transVar ctxt var in
      let e,ety = e_ty @@ transExp ctxt exp in
      begin
        match varloc with
        | LOCAL ->
          Err.error err pos @@ "handler variable " ^ varname var ^ " is read-only";
        | STORE -> 
          ()
      end;
      checkAssignable (raiseTo ety pc) varty err pos;
      fromBase @@ BindCmd{var;exp=e}, ST.empty, 0, 0
    | InputCmd {var;size} ->
      let var,varty = v_ty @@ transVar ctxt var in
      let chlvl = lookupInternal ctxt pos in
      let size,sizety,sizelvl = e_ty_lvl @@ transExp ctxt size in
      checkAssignable (Ty.Type{base=Ty.STRING;level=L.lub chlvl pc}) varty err pos;
      checkInt sizety err pos;
      checkFlow sizelvl L.bottom err pos;
      fromBase @@ InputCmd{var;size}, ST.empty, 0, 0
    | SendCmd {channel;exp} ->
      let (chlvl,chty,chreach,chcost,choverhead) = lookupRemote ctxt channel pos in
      let e,ety = e_ty @@ transExp ctxt exp in
      checkFlow pc chlvl err pos;
      checkAssignable ety chty err pos;

      let reach, cost, overhead =
        match L.flows_to chlvl L.bottom with
        | true -> ST.empty, 0, 0
        | false ->
          if not @@ ST.mem channel declared_reach
          then Err.error err pos @@ "non-public channel must be declared reachable for sending: " ^ Ch.to_string channel ^ "@" ^ L.to_string chlvl;
          if ST.mem hlchannel chreach
          then Err.error err pos @@ "non-public channel is be recursive: " ^ Ch.to_string channel ^ "@" ^ L.to_string chlvl;
          begin
          match ST.diff chreach declared_reach with
          | s when not (ST.is_empty s) -> Err.error err pos @@ "non-public channel " ^ Ch.to_string channel ^ "@" ^ L.to_string chlvl ^ " reaches undeclared channels " ^ String.concat ", " (List.map Ch.to_string @@ List.of_seq @@ ST.to_seq s);
          | _ -> ()
          end;
          ST.add channel chreach, chcost+1, choverhead in

      fromBase @@ SendCmd{channel;exp=e}, reach, cost, overhead
    | IfCmd{test;thn;els} ->
      let test,testty,testlvl = e_ty_lvl @@ transExp ctxt test in
      checkInt testty err pos;
      checkFlow testlvl L.bottom err pos;
      let (thn, rthn, cthn, othn) = trcmd pc thn in
      let (els, rels, cels, oels) = trcmd pc els in
      let cost = min cthn cels in
      let overhead = max (cthn+othn) (cels+oels) - cost in
      fromBase @@ IfCmd{test;thn;els}, ST.union rthn rels, cost, overhead
    | WhileCmd{test;body} ->
      let test,testty,testlvl = e_ty_lvl @@ transExp ctxt test in
      checkInt testty err pos;
      checkFlow testlvl L.bottom err pos;
      let (body,rbody,cbody,obody) = trcmd pc body in
      let highpc = not @@ L.flows_to pc L.bottom in
      if highpc && not @@ ST.is_empty rbody
      then Err.error err pos @@ "reach for while-loop under non-public pc-label " ^ L.to_string pc ^ " must be empty";
      if highpc && cbody <> 0
      then Err.error err pos @@ "cost for while-loop under non-public pc-label " ^ L.to_string pc ^ " must be 0";
      if highpc && obody <> 0
      then Err.error err pos @@ "overhead for while-loop under non-public pc-label " ^ L.to_string pc ^ " must be 0";
      fromBase @@ WhileCmd{test;body}, rbody, cbody, obody
    | OblivIfCmd{test;thn;els} ->
      let test,testty,testlvl = e_ty_lvl @@ transExp ctxt test in
      checkInt testty err pos;
      let (thn, rthn, cthn, othn) = trcmd (L.lub pc testlvl) thn in
      let (els, rels, cels, oels) = trcmd (L.lub pc testlvl) els in
      let reach = ST.union rthn rels in
      if L.flows_to testlvl L.bottom
      then Err.error err pos @@ "test for oblif is labelled public";
      let cost = min cthn cels in
      let overhead = othn + oels + max cthn cels in
      fromBase @@ OblivIfCmd{test;thn;els}, reach, cost, overhead
    | PrintCmd {info;exp} ->
      let exp = transExp ctxt exp in
      fromBase @@ PrintCmd{info;exp}, ST.empty, 0, 0
    | ExitCmd -> 
      checkLowPC pc err pos;
      fromBase ExitCmd, ST.empty, 0, 0
  in trcmd

let transEffects ctxt hlchannel effects =
  let f (ropt,copt,oopt) = function
    | A.Reach {pos;_} when Option.is_some ropt ->
      Err.error ctxt.err pos @@ "reach already declared for channel " ^ Ch.to_string hlchannel;
      (ropt,copt,oopt)
    | A.Reach {channels; pos} ->
      (Some (ST.of_list channels, pos), copt, oopt)
    | A.Cost {pos;_} when Option.is_some copt ->
      Err.error ctxt.err pos @@ "cost already declared for channel " ^ Ch.to_string hlchannel;
      (ropt,copt,oopt)
    | A.Cost {cost;pos} ->
      (ropt,Some (cost, pos),oopt)
    | A.Overhead {pos;_} when Option.is_some oopt ->
      Err.error ctxt.err pos @@ "overhead already declared for channel " ^ Ch.to_string hlchannel;
      (ropt,copt,oopt)
    | A.Overhead {overhead;pos} ->
      (ropt,copt,Some (overhead,pos)) in

  let (ropt,copt,oopt) = List.fold_left f (None,None,None) effects in

  let g = function
      | A.Reach {channels;pos} -> Reach{channels;pos}
      | A.Cost {cost;pos} -> Cost{cost;pos}
      | A.Overhead {overhead;pos} -> Overhead{overhead;pos} in

  let r, c, o =
    match ropt,copt,oopt with
    | None, None, None ->
      ST.empty, 0, 0
    | None, Some (_,pos), _ ->
      Err.error ctxt.err pos @@ "cost declared for channel with no reach";
      ST.empty, 0, 0
    | None, _, Some (_,pos) ->
      Err.error ctxt.err pos @@ "overhead declared for channel with no reach";
      ST.empty, 0, 0
    | Some (r,_), None, None ->
      r, 0, 0
    | Some (r,_), Some (c,_), Some (o,_) ->
      r, c, o
    | Some (r,_), Some (c,_), None ->
      r, c, 0
    | Some (r,_), None, Some (o,_) ->
      r, 0, o in
  List.map g effects, r, c, o

let transDecl ({gamma;lambda;input;err;_} as ctxt: context) dec =
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
  | A.ChannelDecl {channel;level;effects;ty;pos} ->
    let effects, reach, cost, overhead = transEffects ctxt channel effects in
    H.add lambda channel (level,ty,reach,cost,overhead);
    if ST.mem channel reach && level <> L.bottom
    then Err.error ctxt.err pos @@ "non-public channel is recursive: " ^ Ch.to_string channel ^ "@" ^ L.to_string level;
    ChannelDecl{channel;level;effects;ty;pos}
  | A.InputDecl{level;pos} ->
    begin
    match input with
    | Some _ -> Err.error err pos @@ "input channel already declared"
    | None -> ctxt.input <- Some level
    end;
    InputDecl{level;pos}

let transHl ctxt node (A.Hl{handler;level;effects;x;ty;body;pos}) =
  let hlchannel = Ch.Ch{node;handler} in
  let ctxt = {ctxt with delta = H.create 1024} in
  H.add ctxt.delta x ty;

  let effects, declared_reach, declared_cost, declared_overhead = transEffects ctxt hlchannel effects in

  let (body,reach,cost,overhead) = transCmd ctxt hlchannel declared_reach level body in
  if ST.mem hlchannel reach && level <> L.bottom
  then Err.error ctxt.err pos @@ "non-public channel is recursive: " ^ Ch.to_string hlchannel ^ "@" ^ L.to_string level;
  begin
  match ST.diff declared_reach reach with
  | s when not (ST.is_empty s) -> Err.error ctxt.err pos @@ "unreachable channels declared reachable " ^ String.concat ", " (List.map Ch.to_string @@ List.of_seq @@ ST.to_seq s)
  | _ -> ()
  end;
  if declared_cost <> cost
  then Err.error ctxt.err pos @@ "incorrect cost for " ^ Ch.to_string hlchannel ^ " -- declared: " ^ Int.to_string declared_cost ^ ", inferred: " ^ Int.to_string cost;
  if declared_overhead <> overhead
  then Err.error ctxt.err pos @@ "incorrect overhead for " ^ Ch.to_string hlchannel ^ " -- declared: " ^ Int.to_string declared_overhead ^ ", inferred: " ^ Int.to_string overhead;
  Hl{handler;level;effects;x;ty;body;pos}

let transProg (A.Prog{node;decls;hls}) =
  let ctxt = 
    { gamma = H.create 1024
    ; lambda = H.create 1024
    ; delta = H.create 0
    ; input = None
    ; err = Err.initial_env
    } in
  let decls = List.map (transDecl ctxt) decls in
  let hls = List.map (transHl ctxt node) hls in
  not (Err.any_errors ctxt.err), Prog{node;decls;hls}
