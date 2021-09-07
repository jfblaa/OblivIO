module T = Thread
module M = Common.Message
module L = Common.Level
module A = Common.Tabsyn

module H = Hashtbl

open Common.Value
open Common.Oper

type handler_info = {var: string; cmd: A.cmd}
type server_info = {input: in_channel; output: out_channel}

type sync_queue =
  { lock: Mutex.t
  ; mutable queue: M.message list
  }

type context = 
  { name: string
  ; message_queue: sync_queue
  ; mutable input_buffer: char array
  ; mutable mode: int list
  ; mem: (string, value) H.t
  ; handlers: (string, handler_info) H.t
  ; trust_map: (string * string, L.level) H.t
  ; server: server_info
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

let safeDiv a b =
  let b0 = Bool.to_int (b = 0) in
  let b' = b*(b0 lxor 1) lor b0 in
  ((a / b')*(b0 lxor 1)) lor (b0*max_int)

let safeConcat (arr1 : char array) (arr2 : char array) =
  let l1 = Array.length arr1 in
  let l2 = Array.length arr2 in
  let len = l1 + l2 in
  let res = Array.make len '\000' in
  let idx = ref 0 in
  let c = ref 0 in
  for i = 0 to len-1 do
    if i < l1
    then c := Char.code @@ arr1.(i);
    for j = 0 to l2-1 do
      let v = Char.code @@ arr2.(j) in
      let c' = Bool.to_int (j = !idx) land Bool.to_int (!c = 0) in
      idx := !idx + Bool.to_int (c' <> 0);
      c := !c lor (v*c')
    done;
    res.(i) <- Char.chr !c;
    c := 0
  done;
  res
  

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
  let mismatch = ref 0 in
  let len, chk_opt =
    match Array.length arr1, Array.length arr2 with
    | l1, l2 when l1 < l2 -> l1, Some arr2
    | l1, l2 when l1 > l2 -> l2, Some arr1
    | l1, _ -> l1, None in
  for i = 0 to len-1 do
    let i1 = Char.code @@ arr1.(i) in
    let i2 = Char.code @@ arr1.(i) in
    mismatch := (i1 lxor i2) lor !mismatch
  done;
  (match chk_opt with
  | Some arr ->
    let i1 = Char.code @@ arr.(len) in
    let i2 = Char.code '\000' in
    mismatch := (i1 lxor i2) lor !mismatch
  | _ -> ());
  Bool.to_int (!mismatch = 0)

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
    let ai = (Bool.to_int @@ (a > 0)) in
    let bi = (Bool.to_int @@ (b > 0)) in
    IntVal (Bool.to_int @@ (ai land bi > 0))
  | OrOp, IntVal a, IntVal b ->
    let ai = (Bool.to_int @@ (a > 0)) in
    let bi = (Bool.to_int @@ (b > 0)) in
    IntVal (Bool.to_int @@ (ai lor bi > 0))
  | PlusOp, IntVal a, IntVal b ->
    IntVal (a+b)
  | MinusOp, IntVal a, IntVal b ->
    IntVal (a-b)
  | TimesOp, IntVal a, IntVal b ->
    IntVal (a*b)
  | DivideOp, IntVal a, IntVal b ->
    IntVal (safeDiv a b)
  (* STRING *)
  | EqOp, StringVal a, StringVal b ->
    IntVal (safeEq a b)
  | NeqOp, StringVal a, StringVal b ->
    IntVal ((safeEq a b) lxor 1)
  | CaretOp, StringVal a, StringVal b ->
    StringVal (safeConcat a b)
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
          let bit = ((mode lxor 1) land b1) lor (mode land b2) in
          let mode = mode land b2 in
          let i = ((mode lxor 1) * i1) lor (mode * i2) in
          bit, IntVal i
        | Val{bit=b1;v=StringVal s1}, Val{bit=b2;v=StringVal s2} ->
          let bit = ((mode lxor 1) land b1) lor (mode land b2) in
          let s = safeBind (mode land b2) s1 s2 in
          bit, StringVal s
        | _ -> raise @@ InterpFatal ("type mismatch during bind") in
      H.add ctxt.mem x (Val{bit;v})
    | InputCmd { var=(x,_); size; _ } ->
      let Val{bit;v} = eval ctxt size in
      let n =
        match v with
        | IntVal n -> n
        | _ -> raise @@ InterpFatal __LOC__ in
      let max_len = Array.length ctxt.input_buffer in
      let res_len = min n max_len in
      let blank = Array.make res_len '\000' in
      let upd = Array.sub ctxt.input_buffer 0 res_len in
      let mode = getMode () in
      let res = safeBind (mode land bit) blank upd in
      let resbit = Bool.to_int @@ (res.(0) <> '\000') in

      let buf_upd =
        Array.append
          (Array.sub ctxt.input_buffer res_len (max_len - res_len))
          blank in
      
      ctxt.input_buffer <- safeBind (mode land bit) ctxt.input_buffer buf_upd;

      H.add ctxt.mem x (Val{bit=mode land bit land resbit;v=StringVal res})
    | SendCmd { node; channel; exp } ->
      let level = lookup ctxt.trust_map (node,channel) in
      let Val{bit;v} = eval ctxt exp in
      let mode = getMode () in
      let v =
          match v with
          | IntVal n -> IntVal ((bit land mode)*n)
          | StringVal s ->
            StringVal (safeBind (bit land mode) (Array.make (Array.length s) '\000') s) in
      let value = Val{bit=bit land mode;v} in
      let msg = M.Relay{sender=ctxt.name;receiver=node;channel;level;value} in
      output_value ctxt.server.output msg;
      flush ctxt.server.output
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
        | IntVal n -> Bool.to_int @@ (n <> 0)
        | _ -> 1 in
      let mode = getMode () in
      ctxt.mode <- i land mode :: (i lxor 1) land mode :: ctxt.mode;
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
      if mode land bit = 1 then print_endline @@ intro ^ base_to_string v;
      in
  _I

let rec loop ctxt =
  begin
  match input_value ctxt.server.input with
  | M.Relay{channel;value;_} ->
    begin
      match H.find_opt ctxt.handlers channel with
      | Some {var;cmd} ->
        H.add ctxt.mem var value;
        interpCmd ctxt cmd
      | None -> ()
    end
  | _ -> ()
  end;
  T.yield ();
  loop ctxt

let rec prompt ctxt () =
  let line = read_line () in
  let arr = line |> String.to_seq |> Array.of_seq in
  let l1 = Array.length arr in
  let l2 = Array.length ctxt.input_buffer in
  Array.blit arr 0 ctxt.input_buffer 0 (min l1 l2);
  prompt ctxt ()

let interp (A.Prog{node;decls;chs}) =
  let inet_addr = Unix.inet_addr_of_string "127.0.0.1" in
  let sockaddr = Unix.ADDR_INET (inet_addr,3050) in
  let input,output = Unix.open_connection sockaddr in

  let ctxt =
    { name = node
    ; message_queue = 
      { lock = Mutex.create ()
      ; queue = []
      }
    ; input_buffer = Array.make 256 '\000'
    ; mode = [1]
    ; mem = H.create 1024
    ; handlers = H.create 1024
    ; trust_map = H.create 1024
    ; server = {input;output}
    } in
  let f (A.Ch{ch;var=(var,_);body;_}) =
    H.add ctxt.handlers ch {var;cmd=body} in
  let g = function
    | (A.VarDecl{var=(var,_);init;padding=None;_}) ->
      let i = eval ctxt init in
      H.add ctxt.mem var i
    | (A.VarDecl{var=(var,_);init;padding=Some pad;_}) ->
      let i = eval ctxt init in
      let p = eval ctxt pad in
      begin
      match i, p with
      | Val{v=StringVal orig;_}, Val{v=IntVal pad;_} ->
        let upd = Array.make pad '\000' in
        let v = StringVal (safeBind 0 orig upd) in
        H.add ctxt.mem var (Val{bit=1;v})
      | _ -> raise @@ InterpFatal __LOC__
      end
    | (A.InputDecl _) ->
      ()
    | (A.ChannelDecl{node;ch;ty;_}) ->
      let lvl = Common.Types.level ty in
      H.add ctxt.trust_map (node,ch) lvl in
  
  List.iter f chs;
  List.iter g decls;

  output_value ctxt.server.output (M.Greet {sender=ctxt.name});
  flush ctxt.server.output;

  let _ = T.create (prompt ctxt) () in

  loop ctxt
