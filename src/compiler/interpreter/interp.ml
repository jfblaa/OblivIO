open Common

module A = Tabsyn
module O = Oper
module L = Level
module V = Value
module M = Message

module H = Hashtbl

type handler_info = {ty: Types.ty; var: A.var; prelude: A.cmd; body: A.cmd; pos: Lexing.position}
type node_state = ConsumerState | ProducerState of A.cmd

type prog_config
  = { node: string
    ; adv: L.level option
    ; handlers: (string, handler_info) H.t
    ; chdecls: (string, L.level) H.t
    ; mutable state: node_state
    ; mutable mem: (A.var, V.value) H.t
    ; mutable msg_queue: M.message list
    }

type node_table = (string, prog_config)  H.t
type channel_table = (string, prog_config)  H.t

exception InterpFatal
exception NotImplemented of string

let lookup m x =
  match H.find_opt m x with
  | Some v -> v
  | None -> raise InterpFatal

let op oper (v1,s1) (v2,s2) =
  match oper,v1,v2 with
  (* INT *)
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
  (* STRING *)
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
    let _regular base = V.Regular (base, V.size_of_base base) in
    match exp_base with
    | A.IntExp i -> _regular @@ V.IntVal i
    | A.StringExp s -> _regular @@ V.StringVal s
    | A.VarExp x -> lookup mem x
    | A.ProjExp exp -> 
      begin
      match _E exp with
      | V.Obliv (_,v,z) -> V.Regular (v,z)
      | _ -> raise InterpFatal
      end
    | A.InjExp exp ->
      begin
      match _E exp with
      | V.Regular (v,z) -> V.Obliv (true,v,z)
      | _ -> raise InterpFatal
      end
    | A.SizeExp exp ->
      begin
      match _E exp with
      | V.Regular (_,s) -> _regular @@ V.IntVal s
      | _ -> raise InterpFatal
      end
    | A.OpExp {left;oper;right} ->
      let lval = _E left in
      let rval = _E right in
      begin
      match lval,rval with
      | V.Regular (v1,s1), V.Regular (v2,s2) ->
        let v, s = op oper (v1,s1) (v2,s2) in
        V.Regular (v,s)
      | _ -> raise InterpFatal
      end
  in _E

let producer_step (ctxt: prog_config) cmd =
  let doneby msg =
    ctxt.state <- ConsumerState;
    msg in
  let continue cmd msg =
    ctxt.state <- ProducerState cmd;
    msg in
  let rec _P phantom (A.Cmd{cmd_base;pos}) =
    let (~>) cmd_base = A.Cmd{cmd_base;pos} in
    match cmd_base with
    | AssignCmd {var;exp} when phantom ->
      begin 
      match eval ctxt exp with
      | V.Obliv (_,v,z) ->
        H.add ctxt.mem var (V.Obliv (false,v,z));
        doneby []
      | V.Regular _ ->
        doneby []
      end
    | AssignCmd {var;exp} ->
      begin 
      match eval ctxt exp with
      | V.Obliv (b,v,z) ->
        H.add ctxt.mem var (V.Obliv (b,v,z));
        doneby []
      | rv ->
        H.add ctxt.mem var rv;
        doneby []
      end
    | OblivAssignCmd {var;exp} when phantom ->
      begin 
      match lookup ctxt.mem var, eval ctxt exp with
      | V.Regular (v1,z1),V.Obliv (_,_,z2) ->
        let z = max z1 z2 in
        H.add ctxt.mem var (V.Regular (v1,z));
        doneby []
      | _ ->
        raise InterpFatal
      end
    | OblivAssignCmd {var;exp} ->
      begin 
      match lookup ctxt.mem var, eval ctxt exp with
      | V.Regular (_,z1),V.Obliv (b,v2,z2) when b ->
        let z = max z1 z2 in
        H.add ctxt.mem var (V.Regular (v2,z));
        doneby []
      | V.Regular (v1,z1),V.Obliv (_,_,z2) ->
        let z = max z1 z2 in
        H.add ctxt.mem var (V.Regular (v1,z));
        doneby []
      | _ ->
        raise InterpFatal
      end
    | SeqCmd {c1;c2} ->
      let msg = _P phantom c1 in
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
      | V.Regular (V.IntVal 0,_) -> continue els []
      | _ -> continue thn []
      end
    | OblivIfCmd {thn;els;_} when phantom ->
        continue ~>(A.PhantomCmd (~> (A.SeqCmd{c1 = thn; c2 = els}))) []
    | OblivIfCmd {test;thn;els} ->
      begin match eval ctxt test with
      | V.Regular (V.IntVal 0, _) ->
        continue (~> (A.SeqCmd{c1 = ~>(A.PhantomCmd thn); c2 = els})) []
      | _ ->
        continue (~> (A.SeqCmd{c1 = thn; c2 = ~>(A.PhantomCmd els)})) []
      end
    | WhileCmd {test;body} ->
      let skipcmd = ~>(A.SkipCmd) in
      let thn = ~>(A.SeqCmd{c1=body;c2=cmd}) in
      continue (~> (A.IfCmd{test;thn;els=skipcmd})) []
    | OutputCmd {channel;exp} when phantom ->
      let z =
        match eval ctxt exp with
        | V.Obliv _ -> raise InterpFatal
        | V.Regular (_,z) -> z in
      doneby
      [ M.Msg
        { sender = ctxt.node
        ; channel
        ; packet = M.Dummy z
        } ]
    | OutputCmd {channel;exp} ->
      let rv =
        match eval ctxt exp with
        | V.Obliv _ -> raise InterpFatal
        | rv -> rv in
      doneby
      [ M.Msg
        { sender = ctxt.node
        ; channel
        ; packet = M.Real rv
        } ]
    | PrintCmd { info; exp } ->
      let v = eval ctxt exp in
      let intro =
        match info with
        | Some s -> s ^ ": "
        | None -> "" in
      if not phantom then print_endline @@ "(" ^ ctxt.node ^ ") " ^ intro ^ V.to_string v;
      doneby []
    | PhantomCmd cmd' ->
      let msg = _P true cmd' in
      begin
      match ctxt.state with
      | ConsumerState ->
        doneby msg
      | ProducerState c ->
        continue (~> (A.PhantomCmd c)) msg
      end
  in _P false cmd

let consumer_step (({handlers;msg_queue;_} as ctxt): prog_config) =
  match msg_queue with
  | [] -> ConsumerState
  | M.Msg{channel;packet;_}::qs ->
    ctxt.msg_queue <- qs;
    match H.find_opt handlers channel with
    | None ->
      print_endline @@ "WARNING: no handler is associated with channel" ^ channel;
      ConsumerState
    | Some {ty;var;prelude;body;pos} ->
      let _pos (A.Cmd{pos;_}) = pos in
      let _seq c1 c2 = A.Cmd{cmd_base=A.SeqCmd{c1;c2};pos} in
      let v,body =
        match Types.base ty,packet with
        | _,Real (V.Regular (v,z)) ->
          V.Obliv (true,v,z),body 
        | Types.INT, Dummy z ->
          V.Obliv (false,V.default_base_int,z),
          A.Cmd{cmd_base=A.PhantomCmd body;pos = _pos body}
        | Types.STRING, Dummy z ->
          V.Obliv (false,V.default_base_int,z),
          A.Cmd{cmd_base=A.PhantomCmd body;pos = _pos body}
        | _ -> raise InterpFatal in

      H.add ctxt.mem var v;
      ProducerState (_seq prelude body)

let step_node (ctxt: prog_config) =
  match ctxt.state with
  | ConsumerState -> 
    ctxt.state <- consumer_step ctxt;
    []
  | ProducerState cmd ->
    producer_step ctxt cmd

let receiver_node channel sender_node chtbl =
  match H.find_opt sender_node.handlers channel, H.find_opt chtbl channel with
  | Some _, _ ->
    Some sender_node
  | _, Some node ->
    Some node
  | None,None ->
    print_endline @@ "WARNING: no channel with name" ^ channel;
    None

let string_of_msg_out cfg (Message.Msg{channel;_} as msg) =
  match cfg.adv with
  | None -> None
  | Some ladv ->
    let lmsg =
      match H.find_opt cfg.handlers channel, H.find_opt cfg.chdecls channel with
      | Some {ty=Types.Type{level;_};_},_ -> level
      | _, Some level -> level
      | _ ->
        print_endline @@ "WARNING: no level declared for channel " ^ channel ^ " at node " ^ cfg.node;
        L.bottom in
    if L.flows_to lmsg ladv
    then Some (cfg.node ^ " -> " ^ M.to_string msg)
    else Some (cfg.node ^ " -> " ^ M.to_string_hidden msg)

let string_of_msg_in cfg (Message.Msg{channel;_} as msg) =
  match cfg.adv with
  | None -> None
  | Some ladv ->
    let lmsg =
      match H.find_opt cfg.handlers channel with
      | Some {ty=Types.Type{level;_};_} -> level
      | _ ->
        print_endline @@ "WARNING: no level declared for channel " ^ channel ^ " at node " ^ cfg.node;
        L.bottom in
    if L.flows_to lmsg ladv
    then Some (cfg.node ^ " <- " ^ M.to_string msg)
    else Some (cfg.node ^ " <- " ^ M.to_string_hidden msg)

let rec step_sys time (ntbl:node_table) (chtbl:channel_table) nodes =
  let f acc node = 
    step_node node @ acc in
  let round_msgs = List.fold_left f [] nodes in
  let g (M.Msg{sender;channel;_} as msg) =
    let sender_node = H.find ntbl sender in
    let channel_node = Option.get @@ receiver_node channel sender_node chtbl in
    (match string_of_msg_out sender_node msg with
    | Some s ->
      print_endline @@  "Time " ^ Int.to_string time ^ ": " ^ s
    | None -> ());
    (match string_of_msg_in channel_node msg with
    | Some s ->
      print_endline @@ "Time " ^ Int.to_string time ^ ": " ^ s
    | None -> ());
    channel_node.msg_queue <- channel_node.msg_queue @ [msg] in
  List.iter g round_msgs;
  let h node =
    match node.state, List.length node.msg_queue with
    | ConsumerState, 0 -> false
    | _ -> true in
  if List.exists h nodes
  then step_sys (time + 1) ntbl chtbl nodes

let interp (progs: A.program list) =
  let ntbl = H.create 1024 in
  let chtbl = H.create 1024 in
  let setup (A.Prog{node;adv;decls;init;chs}) =
    let state =
      match init with
      | Some cmd -> ProducerState cmd
      | None -> ConsumerState in
    let ctxt =
      { node
      ; adv
      ; handlers = H.create 1024
      ; chdecls = H.create 1024
      ; state
      ; mem = H.create 1024
      ; msg_queue = []
      } in
    let f (A.Ch{ty;name;var;prelude;body;pos}) =
      let hn_info = {ty;var;prelude;body;pos} in
      H.add chtbl name ctxt;
      H.add ctxt.handlers name hn_info in
    let g = function
      | (A.VarDecl{var;init;_}) ->
        let i = eval ctxt init in
        H.add ctxt.mem var i;
      | (A.ChDecl{name;ty;_}) ->
        let lvl = Types.level ty in
        H.add ctxt.chdecls name lvl in
    H.add ntbl node ctxt;
    List.iter f chs;
    List.iter g decls;
    ctxt
    in
  step_sys 0 ntbl chtbl @@ List.map setup progs
