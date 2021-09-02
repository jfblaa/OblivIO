module T = Thread
module M = Common.Message
module L = Common.Level
module A = Common.Tabsyn

module H = Hashtbl

open Common.Value
open Common.Oper

type handler_info = {var: string; cmd: A.cmd}

type sync_queue =
  { lock: Mutex.t
  ; mutable queue: M.message list
  }

type context = 
  { name: string
  ; message_queue: sync_queue
  ; input_queue: sync_queue
  ; mutable mode: int list
  ; mutable mem: (string, value) H.t
  ; mutable handlers: (string, handler_info) H.t
  ; mutable trust_map: (string * string, L.level) H.t
  ; mutable network: (string, context) H.t
  }

let enqueue (q: sync_queue) (msg: M.message) =
  Mutex.lock q.lock;
  q.queue <- q.queue @ [msg];
  Mutex.unlock q.lock

let dequeue (q: sync_queue) =
  Mutex.lock q.lock;
  let msg_opt =
    match q.queue with
    | msg::ls ->
      q.queue <- ls;
      Some msg
    | [] ->
      None in
  Mutex.unlock q.lock;
  msg_opt

exception InterpFatal of string
exception NotImplemented of string
  
let lookup m x =
  match H.find_opt m x with
  | Some v -> v
  | None -> raise @@ InterpFatal ("lookup")

let safeBind (bit : int) (orig : char array) (upd : char array) =
  let len, padded_orig, padded_upd =
    match Array.length orig, Array.length upd with
      | l1, l2 when l1 < l2 ->
        l2,
        Array.append orig @@ Array.make (l2-l1) '\000',
        upd
      | l1, l2 when l1 > l2 ->
        l1,
        orig,
        Array.append upd @@ Array.make (l1-l2) '\000'
      | l1, _ ->
        l1, orig, upd in
  let res = Array.make len '\000' in
  for i = 0 to len-1 do
    let i1 = (1 lxor bit) * (Char.code @@ padded_orig.(i)) in
    let i2 = bit * (Char.code @@ padded_upd.(i)) in
    res.(i) <- Char.chr @@ i1 lor i2
  done;
  res

let safeEq (arr1 : char array) (arr2 : char array) =
  let matches = ref 0 in
  let len, chk_opt =
    match Array.length arr1, Array.length arr2 with
    | l1, l2 when l1 < l2 -> l1, Some arr2
    | l1, l2 when l1 > l2 -> l2, Some arr1
    | l1, _ -> l1, None in
  for i = 0 to len-1 do
    let i1 = Char.code @@ arr1.(i) in
    let i2 = Char.code @@ arr1.(i) in
    matches := i1 lxor i2 lor !matches
  done;
  (match chk_opt with
  | Some arr ->
    let i1 = Char.code @@ arr.(len) in
    let i2 = Char.code '\000' in
    matches := i1 lxor i2 lor !matches
  | _ -> ());
  Bool.to_int (!matches = 0)

let op oper v1 v2 =
  match oper,v1,v2 with
  (* INT *)
  | EqOp, IntVal a, IntVal b ->
    IntVal (Bool.to_int @@ (a lxor b = 0))
  | NeqOp, IntVal a, IntVal b ->
    IntVal (Bool.to_int @@ (a lxor b <> 0))
  | LtOp, IntVal a, IntVal b ->
    IntVal (Bool.to_int @@ (a - b < 0))
  | LeOp, IntVal a, IntVal b ->
    IntVal (Bool.to_int @@ (a - b <= 0))
  | GtOp, IntVal a, IntVal b ->
    IntVal (Bool.to_int @@ (b - a < 0))
  | GeOp, IntVal a, IntVal b ->
    IntVal (Bool.to_int @@ (b - a <= 0))
  | AndOp, IntVal a, IntVal b ->
    IntVal (Bool.to_int @@ (a land b > 0))
  | OrOp, IntVal a, IntVal b ->
    IntVal (Bool.to_int @@ (a lor b > 0))
  | PlusOp, IntVal a, IntVal b ->
    IntVal (a+b)
  | MinusOp, IntVal a, IntVal b ->
    IntVal (a-b)
  (* STRING *)
  | EqOp, StringVal a, StringVal b ->
    IntVal (safeEq a b)
  | NeqOp, StringVal a, StringVal b ->
    IntVal ((safeEq a b) lxor 1)
  | _ -> raise @@ NotImplemented "binop"

let eval {mem;_} =
  let rec _E (A.Exp{exp_base;_}) =
    match exp_base with
    | A.IntExp i -> Val{bit=1;v=IntVal i}
    | A.StringExp s -> 
      let arr = Array.of_seq @@ String.to_seq s in
      Val{bit=1;v=StringVal arr}
    | A.VarExp (x,_) -> lookup mem x
    | A.QuestionExp exp -> 
      let Val{bit;_} = _E exp in
      Val{bit=1;v=IntVal bit}
    | A.SizeExp exp ->
      let v = _E exp in
      Val{bit=1;v=IntVal (sizeof v)}
    | A.OpExp {left;oper;right} ->
      let Val{bit=b1;v=v1} = _E left in
      let Val{bit=b2;v=v2} = _E right in
      let v = op oper v1 v2 in
      Val{bit=b1 land b2;v}
  in _E

let interpCmd ctxt =
  let getMode () =
    match ctxt.mode with
    | m::_ -> m
    | [] -> raise @@ InterpFatal "getMode: stack empty" in
  let rec _I (A.Cmd{cmd_base;pos}) =
    match cmd_base with
    | SkipCmd -> ()
    | SeqCmd {c1;c2} ->
      _I c1;
      _I c2
    | AssignCmd { var=(x,_); exp } ->
      let v = eval ctxt exp in
      H.add ctxt.mem x v
    | BindCmd { var=(x,_); exp } ->
      let orig = lookup ctxt.mem x in
      let upd = eval ctxt exp in
      let mode = getMode () in
      let bit,v =
        match orig,upd with
        | Val{bit=b1;v=IntVal i1}, Val{bit=b2;v=IntVal i2} ->
          let bit = ((mode lxor 1)*b1) lor (mode*b2) in
          let i = ((mode lxor 1)*i1) lor (mode*i2) in
          bit, IntVal i
        | Val{bit=b1;v=StringVal s1}, Val{bit=b2;v=StringVal s2} ->
          let bit = ((mode lxor 1)*b1) lor (mode*b2) in
          let s = safeBind mode s1 s2 in
          bit, StringVal s
        | _ -> raise @@ InterpFatal ("type mismatch during bind") in
      H.add ctxt.mem x (Val{bit;v})
    | InputCmd _ (*{ var; ch; size }*) ->
      raise @@ NotImplemented "InputCmd"
    | SendCmd { node; channel; exp } ->
      let level = lookup ctxt.trust_map (node,channel) in
      let value = eval ctxt exp in
      let queue = (lookup ctxt.network node).message_queue in
      let msg = M.Msg{sender=ctxt.name;receiver=node;channel;level;value} in
      enqueue queue msg
    | IfCmd { test; thn; els } ->
      let Val{v;_} = eval ctxt test in
      begin
      match v with
      | IntVal 0 -> _I els
      | _ -> _I thn
      end
    | WhileCmd { test; body } ->
      while
        let Val{v;_} = eval ctxt test in
        match v with
        | IntVal 0 -> false
        | _ -> true
      do
        _I body
      done
    | OblivIfCmd { test; thn; els } ->
      let Val{v;_} = eval ctxt test in
      let i =
        match v with
        | IntVal 0 -> 0
        | _ -> 1 in
      let mode = getMode () in
      ctxt.mode <- i*mode :: (i lxor 1)*mode :: ctxt.mode;
      let (~>) cmd_base = A.Cmd{cmd_base;pos} in
      let _S c1 c2 = A.Cmd{cmd_base=A.SeqCmd{c1;c2};pos} in
      let cmd' = _S thn @@ _S (~> A.PopCmd) @@ _S els (~> A.PopCmd) in
      _I cmd'
    | PopCmd ->
      begin
      match ctxt.mode with
      | _::bits -> ctxt.mode <- bits
      | [] -> raise @@ InterpFatal ("PopCmd: stack empty")
      end
    | PrintCmd { info; exp } ->
      let mode = getMode () in
      let Val{bit;v} = eval ctxt exp in
      let intro =
        match info with
        | Some s -> s ^ ": "
        | None -> "" in
      if mode land bit = 1 then print_endline @@ "(" ^ ctxt.name ^ ") " ^ intro ^ base_to_string v;
      in
  _I

(*let dummy_node =
  { name = "dummy"
  ; message_queue = 
    { lock = Mutex.create ()
    ; queue = []
    }
  ; input_queue = 
    { lock = Mutex.create ()
    ; queue = []
    }
  ; mode = []
  ; mem = H.create 1024
  ; handlers = H.create 1024
  ; trust_map = H.create 1024
  ; network = H.create 1024
  }*)

let rec loop ctxt =
  let _ = match dequeue ctxt.message_queue with
    | Some (M.Msg{channel;value;_}) ->
      begin
        match H.find_opt ctxt.handlers channel with
        | Some {var;cmd} ->
          H.add ctxt.mem var value;
          interpCmd ctxt cmd
        | None -> ()
      end
    | None -> () in
  T.yield ();
  loop ctxt

let interp (progs: A.program list) =
  let network = H.create 1024 in
  let setup (A.Prog{node;decls;chs}) =
    let ctxt =
      { name = node
      ; message_queue = 
        { lock = Mutex.create ()
        ; queue = []
        }
      ; input_queue = 
        { lock = Mutex.create ()
        ; queue = []
        }
      ; mode = [1]
      ; mem = H.create 1024
      ; handlers = H.create 1024
      ; trust_map = H.create 1024
      ; network
      } in
    let f (A.Ch{ch;var=(var,_);body;_}) =
      H.add ctxt.handlers ch {var;cmd=body} in
    let g = function
      | (A.VarDecl{var=(var,_);init;_}) ->
        let i = eval ctxt init in
        H.add ctxt.mem var i;
      | (A.InternalDecl _) ->
        raise @@ NotImplemented "InternalDeck"
      | (A.RemoteDecl{node;ch;ty;_}) ->
        let lvl = Common.Types.level ty in
        H.add ctxt.trust_map (node,ch) lvl in
    
    H.add network node ctxt;
    List.iter f chs;
    List.iter g decls;
    ctxt in
  let ctxts = List.map setup progs in
  let _ = List.map (T.create loop) ctxts in
  let startMsg = M.Msg{sender="root";receiver="";channel="START";level=L.bottom;value=Val{bit=1;v=IntVal 0}} in
  List.iter (fun ctxt -> enqueue ctxt.message_queue startMsg) ctxts;
  T.delay 10.0;
  ()

  (*
let dummy_node =
  { name = "dummy"
  ; message_queue = 
    { lock = Mutex.create ()
    ; queue = []
    }
  ; input_queue = 
    { lock = Mutex.create ()
    ; queue = []
    }
  ; mode = []
  ; mem = H.create 1024
  ; handlers = H.create 1024
  ; trust_map = H.create 1024
  ; network = H.create 1024
  }

  let start = Sys.time()

let producer x =
  let bit = 1 in
  let arr1 = Array.make 5 @@ Char.chr (97+x) in
  let arr2 = Array.make 10 @@ Char.chr (65+x) in
  let updbit = Random.int 2 in
  let arr = safeBind updbit arr1 arr2 in
  let v = StringVal arr in
  let value = Val{bit;v} in
  let msg = M.Msg{sender="A";receiver="B";channel="TEST";level=L.bottom;value} in
  enqueue dummy_node.message_queue msg

let continue = ref true
let consumer () = 
  let rec consume () =
    (match dequeue dummy_node.message_queue with
    | Some msg ->
      let timestr = Float.to_string @@ Sys.time() -. start |> fun x -> x ^ " " in
      print_endline @@ timestr ^ M.to_string_at_level msg L.bottom
    | None -> ());
    T.yield ();
    if !continue then consume () in
  consume ()

let interp _ =
  print_endline "interperter starting...";
  for i = 0 to 5 do
    ignore(T.create producer i)
  done;
  let _ = T.create consumer () in
  T.delay 1.0;
  continue := false*)