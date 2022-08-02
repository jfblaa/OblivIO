open Common
open Tabsyn
open Oper

module A = Absyn
module Err = Errorenv
module Ty = Types
module L = Level
module Ch = Channel

module H = Hashtbl

module C = Map.Make(Channel)

type capability = int C.t

type context 
  = { gamma: (string, Ty.ty) H.t
    ; lambda: (Ch.channel, (L.level * Ty.ty * capability)) H.t
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
  | None -> L.bottom, errTy err pos @@ "undeclared channel " ^ Ch.to_string ch, C.empty

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

let join (f: int -> int -> int) (a: int C.t) (b: int C.t) =
  let g _ xo yo =
    match xo, yo with
    | Some x, Some y -> Some (f x y)
    | Some x, None -> Some x
    | None, Some y -> Some y
    | None, None -> None in
  C.merge g a b

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

let transCmd ({err;_} as ctxt) =
  let rec trcmd pc (cap: capability) (A.Cmd{cmd_base;pos}): cmd * capability =
    let fromBase cmd_base = Cmd{cmd_base;pos} in
    match cmd_base with
    | SkipCmd -> fromBase SkipCmd, C.empty
    | SeqCmd {c1;c2} ->
      let (c1,cap') = trcmd pc cap c1 in
      let (c2,cap'') = trcmd pc cap' c2 in
      fromBase @@ SeqCmd {c1;c2}, cap''
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
      fromBase @@ AssignCmd{var;exp=e}, cap
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
      fromBase @@ BindCmd{var;exp=e}, cap
    | InputCmd {var;size} ->
      let var,varty = v_ty @@ transVar ctxt var in
      let chlvl = lookupInternal ctxt pos in
      let size,sizety,sizelvl = e_ty_lvl @@ transExp ctxt size in
      checkAssignable (Ty.Type{base=Ty.STRING;level=L.lub chlvl pc}) varty err pos;
      checkInt sizety err pos;
      checkFlow sizelvl L.bottom err pos;
      fromBase @@ InputCmd{var;size}, cap
    | SendCmd {channel;exp} ->
      let (chlvl,chty,chcapability) = lookupRemote ctxt channel pos in
      let e,ety = e_ty @@ transExp ctxt exp in
      checkFlow pc chlvl err pos;
      checkAssignable ety chty err pos;

      let capabilityNeeded = match L.flows_to chlvl L.bottom with
        | true -> C.empty
        | false -> C.update channel (fun io -> Some(Option.fold ~none:1 ~some:(fun i -> i+1) io)) chcapability in

      let capabilityLeft = join (-) cap capabilityNeeded in

      begin
      let _s (ch,i) = Ch.to_string ch ^ ": " ^ Int.to_string (abs i) in
      let negativeCapabilities = C.filter (fun _ i -> i < 0) @@ join (-) cap capabilityNeeded in
      if not @@ C.is_empty negativeCapabilities
      then Err.error ctxt.err pos @@ "capability for sending on channel " ^ Ch.to_string channel ^ " [" ^ String.concat ", " (List.map _s @@ List.of_seq @@ C.to_seq negativeCapabilities) ^ "]";
      end;

      fromBase @@ SendCmd{channel;exp=e}, capabilityLeft
    | IfCmd{test;thn;els} ->
      let test,testty,testlvl = e_ty_lvl @@ transExp ctxt test in
      checkInt testty err pos;
      checkFlow testlvl L.bottom err pos;
      let (thn,cap') = trcmd pc cap thn in
      let (els,cap'') = trcmd pc cap els in
      fromBase @@ IfCmd{test;thn;els}, join min cap' cap''
    | WhileCmd{test;body} ->
      let test,testty,testlvl = e_ty_lvl @@ transExp ctxt test in
      checkInt testty err pos;
      checkLowPC pc err pos;
      checkFlow testlvl L.bottom err pos;
      let (body,_) = trcmd pc C.empty body in
      fromBase @@ WhileCmd{test;body}, cap
    | OblivIfCmd{test;thn;els} ->
      let test,testty,testlvl = e_ty_lvl @@ transExp ctxt test in
      checkInt testty err pos;
      let (thn, cap') = trcmd (L.lub pc testlvl) cap thn in
      let (els, cap'') = trcmd (L.lub pc testlvl) cap' els in
      if L.flows_to testlvl L.bottom
      then Err.error err pos @@ "test for oblif is labelled public";
      fromBase @@ OblivIfCmd{test;thn;els}, cap''
    | PrintCmd {info;exp} ->
      let exp = transExp ctxt exp in
      fromBase @@ PrintCmd{info;exp}, cap
    | ExitCmd -> 
      checkLowPC pc err pos;
      fromBase ExitCmd, cap
  in trcmd

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
  | A.ChannelDecl {channel;level;capability;ty;pos} ->
    let capabilityMap = C.of_seq @@ List.to_seq capability in
    H.add lambda channel (level,ty,capabilityMap);
    if C.mem channel capabilityMap && level <> L.bottom
    then Err.error ctxt.err pos @@ "illegal self capability for declared channel " ^ Ch.to_string channel ^ "@" ^ L.to_string level;
    ChannelDecl{channel;level;capability;ty;pos}
  | A.InputDecl{level;pos} ->
    begin
    match input with
    | Some _ -> Err.error err pos @@ "input channel already declared"
    | None -> ctxt.input <- Some level
    end;
    InputDecl{level;pos}

let transHl ctxt node (A.Hl{handler;level;capability;x;ty;body;pos}) =
  let ctxt = {ctxt with delta = H.create 1024} in
  H.add ctxt.delta x ty;

  let hlchannel = Ch.Ch{node;handler} in

  let capabilityMap = C.of_seq @@ List.to_seq capability in

  if C.mem hlchannel capabilityMap
  then Err.error ctxt.err pos @@ "illegal self capability needed for handler " ^ Ch.to_string hlchannel ^ "@" ^ L.to_string level;

  let (body,_) = transCmd ctxt level capabilityMap body in

  (*let negativeCapabilities = C.filter (fun _ i -> i < 0) cbody in
  begin
  let _s (ch,i) = Ch.to_string ch ^ ": " ^ Int.to_string i in
  if not @@ C.is_empty negativeCapabilities
  then Err.error ctxt.err pos @@ "handler ended up with negative capability... " ^ Ch.to_string hlchannel ^ " [" ^ String.concat ", " (List.map _s @@ List.of_seq @@ C.to_seq negativeCapabilities) ^ "]";
  end;*)

  Hl{handler;level;capability;x;ty;body;pos}

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
