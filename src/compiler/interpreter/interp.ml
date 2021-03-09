open Common

module A = Absyn
module O = Oper
module L = Level
module V = Value
module M = Message

type handler_info = {cmd:A.cmd; var:A.var; sizevar: A.var}
type node_state = ConsumerState | ProducerState of A.cmd

type prog_config
  = { level: L.level
    ; handlers: ((L.level * string) * handler_info) list
    ; mutable hl_vars: (A.var * A.var) option
    ; mutable state: node_state
    ; mutable mem: (A.var * V.value) list
    ; mutable msg_queue: M.message list
    }

type sys_config = (L.level * prog_config) list

exception InterpFatal
exception NotImplemented of string

let lookup m x =
  match List.assoc_opt x m with
  | Some v -> v
  | None -> raise InterpFatal

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
    V.IntVal(if String.equal a b then 1 else 0), 1
  | O.NeqOp,V.StringVal a,V.StringVal b ->
    IntVal(if String.equal a b then 0 else 1), 1
  | O.ConcatOp,V.StringVal a,V.StringVal b ->
    V.StringVal(a^b), s1+s2
  | _ -> raise InterpFatal

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
    | A.VarExp x -> lookup mem x
    | A.OpExp {left;oper;right} ->
      let lval = _E left in
      let rval = _E right in
      op oper lval rval
  in _E

let producer_step (ctxt: prog_config) cmd =
  let doneby msg =
    ctxt.state <- ConsumerState;
    msg in
  let continue cmd msg =
    ctxt.state <- ProducerState cmd;
    msg in
  let rec _P mimic (A.Cmd{cmd_base;pos}) =
  let (~>) cmd_base = A.Cmd{cmd_base;pos} in
  match cmd_base with
  | AssignCmd {var;exp} ->
    let v1,s1 = lookup ctxt.mem var in
    let v2,s2 = eval ctxt exp in
    let v = if mimic then v1 else v2 in
    ctxt.mem <- (var,(v,max s1 s2))::ctxt.mem;
    doneby []
  | SeqCmd {c1;c2} ->
    let msg = _P mimic c1 in
    begin
    match ctxt.state with
    | ConsumerState ->
      continue c2 msg
    | ProducerState c1' ->
      continue (~> (A.SeqCmd{c1=c1';c2})) msg
    end
  | SkipCmd ->
    doneby []
  | IfCmd {test;thn;els} ->
    begin match eval ctxt test with
    | V.IntVal 0, _ -> continue els []
    | _ -> continue thn []
    end
  | OblivIfCmd {test;thn;els} ->
    begin match eval ctxt test with
    | V.IntVal 0, _ ->
      continue (~> (A.SeqCmd{c1 = ~>(A.MimicCmd thn); c2 = els})) []
    | _ ->
      continue (~> (A.SeqCmd{c1 = thn; c2 = ~>(A.MimicCmd els)})) []
    end
  | WhileCmd {test;body} ->
    let skipcmd = ~>(A.SkipCmd) in
    let thn = ~>(A.SeqCmd{c1=body;c2=cmd}) in
    continue (~> (A.IfCmd{test;thn;els=skipcmd})) []
  | SendCmd {level;tag;exp} ->
    let (b,m) = eval ctxt exp in
    let basety = match b with
      | IntVal _ -> Types.INT
      | StringVal _ -> Types.STRING in
    let packet = if mimic then M.Dummy (basety,m) else M.Real (basety,(b,m)) in
    doneby
    [ M.Msg
      { sender = ctxt.level
      ; recipient = level
      ; tag
      ; packet
      } ]
  | PrintCmd { info; exp } ->
    let v = eval ctxt exp in
    let intro =
      match info with
      | Some s -> s ^ ": "
      | None -> "" in
    if not mimic then print_endline @@ intro ^ V.to_string v;
    doneby []
  | MimicCmd cmd' ->
    let msg = _P true cmd' in
    begin
    match ctxt.state with
    | ConsumerState ->
      doneby msg
    | ProducerState c ->
      continue (~> (A.MimicCmd c)) msg
    end
  in _P false cmd

let consumer_step (({handlers;msg_queue;_} as ctxt): prog_config) =
  match msg_queue with
  | [] -> ConsumerState
  | M.Msg{sender;tag;packet;_}::qs ->
    ctxt.msg_queue <- qs;
    match List.assoc_opt (sender,tag) handlers with
    | None ->
      print_endline @@ String.concat " "
      [ "WARNING: no handler associated with"
      ; L.to_string ctxt.level
      ; "accepts messages with tag"
      ; tag
      ; "from"
      ; L.to_string sender
      ];
      ConsumerState
    | Some {cmd=A.Cmd{pos;_} as cmd;var;sizevar} ->
      let v,cmd =
        match packet with
        | Real (_,v) ->
          v, cmd
        | Dummy (Types.INT,m) ->
          (V.default_base_int, m), A.Cmd{cmd_base=A.MimicCmd cmd;pos}
        | Dummy (Types.STRING,m) ->
          (V.default_base_string, m), A.Cmd{cmd_base=A.MimicCmd cmd;pos}
        | Dummy (Types.ERROR,_) -> raise InterpFatal in

      let sval_base = V.IntVal (snd v) in

      let s_val = (sval_base, V.size_of_base sval_base) in
      ctxt.mem <- (sizevar,s_val)::(var,v)::ctxt.mem;
      ctxt.hl_vars <- Some (var,sizevar);
      ProducerState cmd

let step_node (ctxt: prog_config) =
  match ctxt.state with
  | ConsumerState -> 
    ctxt.state <- consumer_step ctxt;
    []
  | ProducerState cmd ->
    let out_msg = producer_step ctxt cmd in
    match ctxt.state with
    | ConsumerState ->
      begin match ctxt.hl_vars with
      | Some (x_var,s_var) ->
        ctxt.mem <- List.remove_assoc s_var ctxt.mem;
        ctxt.mem <- List.remove_assoc x_var ctxt.mem
      | None -> ()
      end;
      out_msg
    | _ -> out_msg

let rec step_sys time nodes =
  let f acc (_,node) = 
    step_node node @ acc in
  let round_msgs = List.fold_left f [] nodes in
  let g (M.Msg{recipient;_} as msg) =
    print_int time;
    print_string ": ";
    print_endline @@ M.to_string_at_level msg L.bottom; (* debug printing to bot! *)
    (*print_endline @@ M.to_string_at_level msg (L.of_list ["Patient";"Doctor";"Clinic";"Customer";"Broker";"Relay";"Alice"]);*)
    match List.assoc_opt recipient nodes with
    | None ->
      print_endline @@ String.concat " "
      [ "WARNING: no recipient with name"
      ; L.to_string recipient
      ; "found"
      ]
    | Some node ->
      node.msg_queue <- node.msg_queue @ [msg] in
  List.iter g round_msgs;
  let h (_,node) =
    match node.state, List.length node.msg_queue with
    | ConsumerState, 0 -> false
    | _ -> true in
  if List.exists h nodes
  then step_sys (time + 1) nodes

let interp (progs: A.program list) =
  let setup (A.Prog{level;vardecls;init;hns}) =
    let f acc (A.Hn{level;tag;sizevar;var;cmd;_}) =
      let hn_info = {cmd;sizevar;var} in
      ((level,tag),hn_info) :: acc in
    let g acc (A.VarDecl{var;basevalue;sizevar;_}) =
      let size = V.size_of_base basevalue in
      let sizeval = V.IntVal size in
      let sizesize = V.size_of_base sizeval in
      (var,(basevalue,size))::(sizevar,(sizeval,sizesize))::acc in
    let state =
      match init with
      | Some cmd -> ProducerState cmd
      | None -> ConsumerState in
    level, { level
    ; handlers = List.fold_left f [] hns
    ; hl_vars = None
    ; state
    ; mem = List.fold_left g [] vardecls
    ; msg_queue = []
    } in
  step_sys 0 @@ List.map setup progs
