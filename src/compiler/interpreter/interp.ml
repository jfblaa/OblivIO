module T = Thread
module M = Common.Message
module L = Common.Level
module A = Common.Tabsyn
module V = Common.Value
module Ty = Common.Types
module C = Common.Channel

module H = Hashtbl

open Common.Value
open Common.Oper

type handler_info = {x: string; replych: (string*Ty.chty) option; decls: A.hldecl list; prelude: A.cmd option; body: A.cmd}
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
  ; mutable replyto: (string*string) option
  ; memory: (string, value) H.t
  ; store: (string, value) H.t
  ; handlers: (string, handler_info) H.t
  ; trust_map: (C.channel, L.level) H.t
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

let _int = function
  | IntVal n -> n
  | _ -> raise @@ InterpFatal "_I"

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
 
let rec safeEq v1 v2 =
  match v1, v2 with
  | IntVal a, IntVal b -> 
    Bool.to_int @@ (a lxor b = 0)
  | StringVal{length=l1;data=d1}, StringVal{length=l2;data=d2} ->
    let mismatch = ref (l1 lxor l2) in
    let publen = min (Array.length d1) (Array.length d2) in
    let seclen = min l1 l2 in
    for i = 0 to publen-1 do
      let bit = Bool.to_int(i < seclen) in
      let i1 = Char.code @@ d1.(i) in
      let i2 = Char.code @@ d2.(i) in
      mismatch := (bit land (i1 lxor i2)) lor !mismatch
    done;
    Bool.to_int (!mismatch = 0)
  | PairVal(a1,a2), PairVal (b1,b2) ->
    safeEq a1 b1 * safeEq a2 b2
  | ArrayVal{length=l1;data=d1}, ArrayVal{length=l2;data=d2} ->
    let mismatch = ref (l1 lxor l2) in
    let publen = min (Array.length d1) (Array.length d2) in
    let seclen = min l1 l2 in
    for i = 0 to publen-1 do
      let bit = Bool.to_int(i < seclen) in
      let i = safeEq d1.(i) d2.(i) in
      mismatch := (bit land (1 lxor i)) lor !mismatch
    done;
    Bool.to_int (!mismatch = 0)
  | _ -> raise @@ NotImplemented "safeEq"

let rec safeSelect (bit: int) (orig: value) (upd: value) =
  let rec _S orig upd =
    match orig, upd with
    | IntVal a, IntVal b ->
      IntVal (((bit lxor 1) * a) lor (bit * b))
    | StringVal{length=l1;data=d1}, StringVal{length=l2;data=d2} ->
      begin
      match Array.length d1, Array.length d2 with
      | arrlen1, arrlen2 when arrlen1 < arrlen2 ->
        let length = ((1 lxor bit)*l1) lor (bit*l2) in
        let data = Array.copy d2 in
        for i = 0 to arrlen1-1 do
          let i1 = (1 lxor bit) * (Char.code @@ d1.(i)) in
          let i2 = bit * (Char.code @@ d2.(i)) in
          data.(i) <- Char.chr @@ i1 lor i2
        done;
        StringVal{length;data}
      | _, arrlen2 ->
        let length = ((1 lxor bit)*l1) lor (bit*l2) in
        let data = Array.copy d1 in
        for i = 0 to arrlen2-1 do
          let i1 = (1 lxor bit) * (Char.code @@ d1.(i)) in
          let i2 = bit * (Char.code @@ d2.(i)) in
          data.(i) <- Char.chr @@ i1 lor i2
        done;
        StringVal{length;data}
      end
    | PairVal (a1,a2), PairVal (b1,b2) ->
      PairVal (_S a1 b1, _S a2 b2)
    | ArrayVal{length=l1;data=d1}, ArrayVal{length=l2;data=d2} ->
      begin
        match Array.length d1, Array.length d2 with
        | arrlen1, arrlen2 when arrlen1 <= arrlen2 ->
          let length = ((1 lxor bit)*l1) lor (bit*l2) in
          let data = Array.copy d2 in
          for i = 0 to arrlen1-1 do
            data.(i) <- _S d1.(i) d2.(i)
          done;
          ArrayVal{length;data}
        | _, arrlen2 ->
          let length = ((1 lxor bit)*l1) lor (bit*l2) in
          let data = Array.copy d1 in
          for i = 0 to arrlen2-1 do
            data.(i) <- safeSelect bit d1.(i) d2.(i)
          done;
          ArrayVal{length;data}
      end
    | _ -> raise @@ InterpFatal "safeBind" in
  _S orig upd

let op oper v1 v2 =
  match oper,v1,v2 with
  (* POLY *)
  | EqOp, _, _ ->
    IntVal (safeEq v1 v2)
  | NeqOp, _, _ ->
    IntVal ((safeEq v1 v2) lxor 1)
  (* INT *)
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
  | CaretOp, StringVal {length=l1;data=d1}, StringVal {length=l2;data=d2} ->
    StringVal {length=l1+l2; data=safeConcat d1 d2}
  | PadOp, s, IntVal length ->
    let data = Array.make length '\000' in
    safeSelect 0 s (StringVal{length;data})
  | _ -> raise @@ NotImplemented (V.to_string v1 ^ to_string oper ^ V.to_string v2)

type update = ASSIGN | BIND
let rec readvar ctxt =
  let rec _V path (A.Var{var_base;loc;_}) = match var_base with
    | A.SimpleVar x ->
      let v = 
        match loc with
        | LOCAL -> lookup ctxt.memory x
        | STORE -> lookup ctxt.store x in
      let rec f path v =
        match path, v with
        | [], _ -> v
        | (i,lvl)::tl, ArrayVal{length;data} ->
          let maxidx = length -1 in
          let cnd1 = Bool.to_int(i >= 0) in
          let cnd2 = Bool.to_int(i > maxidx) in
          let idx = cnd1 * i in
          let idx = ((cnd2 lxor 1) * idx) lor (cnd2 * maxidx) in
          let res =
            if L.flows_to lvl L.bottom
            then f tl data.(idx) (* public index, no problem! *)
            else 
              (* non-public index, must obliv everything! *)
              let len = Array.length data - 1 in
              let res = f tl data.(0) in
              let a = ref res in
              for i = 0 to len do
                let b = f tl data.(i) in
                a := safeSelect (Bool.to_int (i lxor idx = 0)) !a b
              done;
              !a in 
          res
        | _ -> raise @@ InterpFatal "readVar"
        in
      f path v
    | A.SubscriptVar {var;exp} ->
      let A.Exp{ty;_} = exp in
      let i = _int @@ eval ctxt exp in
      let lvl = Ty.level ty in
      _V ((i,lvl)::path) var
in _V []

and writevar ctxt updkind upd mode =
  let rec _V path (A.Var{var_base;loc;_}) = match var_base with
    | A.SimpleVar x ->
      let table =
        match loc with
        | LOCAL -> ctxt.memory
        | STORE -> ctxt.store in
      let v = lookup table x in
      let rec f path v mode =
        match path, v with
        | [], _ ->
          begin 
          match updkind with
          | ASSIGN -> H.add table x upd
          | BIND ->
            if mode = 1
            then
              let orig = lookup table x in
              H.add table x @@ safeSelect mode orig upd
          end
        | [(i,lvl)], ArrayVal{length;data} ->
          let maxidx = length -1 in
          let cnd1 = Bool.to_int(i >= 0) in
          let cnd2 = Bool.to_int(i > maxidx) in
          let idx = cnd1 * i in
          let idx = ((cnd2 lxor 1) * idx) lor (cnd2 * maxidx) in
          if L.flows_to lvl L.bottom
          then (* public index, no problem! *)
            match updkind with
            | ASSIGN ->
              if mode = 1 then data.(idx) <- upd
            | BIND ->
              let orig = lookup table x in
              data.(idx) <- safeSelect mode orig upd
          else 
            (* non-public index, must obliv everything! *)
            let len = Array.length data - 1 in
            for i = 0 to len do
              let right_index = Bool.to_int (i lxor idx = 0) in
              data.(i) <- safeSelect (right_index land mode) data.(i) upd
            done          
        | (i,lvl)::tl, ArrayVal{length;data} ->
          let maxidx = length -1 in
          let cnd1 = Bool.to_int(i >= 0) in
          let cnd2 = Bool.to_int(i > maxidx) in
          let idx = cnd1 * i in
          let idx = ((cnd2 lxor 1) * idx) lor (cnd2 * maxidx) in
          if L.flows_to lvl L.bottom
          then f tl data.(idx) mode (* public index, no problem! *)
          else 
            (* non-public index, must obliv everything! *)
            let len = Array.length data - 1 in
            for i = 0 to len do
              let right_index = Bool.to_int (i lxor idx = 0) in
              f tl data.(i) (right_index land mode)
            done          
        | _ -> raise @@ InterpFatal "writeVar"
        in
      f path v mode
    | A.SubscriptVar {var;exp} ->
      let A.Exp{ty;_} = exp in
      let i = _int @@ eval ctxt exp in
      let lvl = Ty.level ty in
      _V ((i,lvl)::path) var
in _V []

and eval ctxt =
  let rec _E (A.Exp{exp_base;_}) =
    match exp_base with
    | A.IntExp i -> IntVal i
    | A.StringExp s -> 
      let length = String.length s in
      let data = s |> String.to_seq |> Array.of_seq in
      StringVal {length;data}
    | A.VarExp v -> 
      readvar ctxt v
    | A.ProjExp {proj;exp} ->
      let v = _E exp in
      begin
        match proj,v with
        | A.Fst, PairVal (a,_) -> a
        | A.Snd, PairVal (_,b) -> b
        | _ -> raise @@ InterpFatal __LOC__
      end
    | A.LengthExp{public;var} ->
      begin
        match readvar ctxt var with
        | StringVal{length;data} ->
          if public then IntVal (Array.length data) else IntVal length
        | ArrayVal{length;data} ->
          if public then IntVal (Array.length data) else IntVal length
        | _ -> raise @@ InterpFatal "LengthExp"
      end
    | A.SizeExp exp ->
      let v = _E exp in
      IntVal (size v)
    | A.OpExp {left;oper;right} ->
      let v1 = _E left in
      let v2 = _E right in
      let v = op oper v1 v2 in
      v
    | A.PairExp (a,b) ->
      PairVal (_E a,_E b)
    | ArrayExp arr ->
      let length = List.length arr in
      let data =
        arr |> List.map _E
            |> Array.of_list  in
      ArrayVal {length;data}
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
    | AssignCmd { var; exp } ->
      let Var{loc;_} = var in
      begin 
        match loc, getMode () with
        | STORE, 0 -> ()
        | _ ->
          let v = eval ctxt exp in
          writevar ctxt ASSIGN v 1 var
      end
    | BindCmd { var; exp } ->
      let v = eval ctxt exp in
      writevar ctxt BIND v (getMode ()) var
    | InputCmd { var; size; _ } ->
      let ne = _int @@ eval ctxt size in
      let max_len = Array.length ctxt.input_buffer in
      let len = min ne max_len in
      let data = Array.sub ctxt.input_buffer 0 len in
      let updbit = Bool.to_int @@ (data.(0) <> '\000') in
      let shouldBind = getMode () land updbit in
      let str = StringVal{length=Array.length data;data} in
      writevar ctxt BIND str shouldBind var;

      let blank = Array.make len '\000' in
      let buf_upd =
        Array.append
          (Array.sub ctxt.input_buffer len (max_len - len))
          blank in
      let s1 = StringVal{length=max_len;data=ctxt.input_buffer} in
      let s2 = StringVal{length=max_len;data=buf_upd} in
      begin
        match safeSelect shouldBind s1 s2 with
        | StringVal{data;_} ->
          ctxt.input_buffer <- data
        | _ -> raise @@ InterpFatal "InputCmd"
      end
      
    | SendCmd { channel; replyto; exp } ->
      let level = lookup ctxt.trust_map channel in
      let value = eval ctxt exp in
      let bit = getMode () in
      let receiver, channel =
        match channel with
        | Explicit (node,ch) -> node, ch
        | Implicit _ -> Option.get ctxt.replyto in
      let msg = M.Relay{sender=ctxt.name;replyto;receiver;channel;level;bit;value} in
      output_value ctxt.server.output msg;
      flush ctxt.server.output
    | IfCmd { test; thn; els } ->
      begin
      match eval ctxt test with
      | IntVal 0 -> _I els
      | _ -> _I thn
      end
    | WhileCmd { test; body } ->
      while
        match eval ctxt test with
        | IntVal 0 -> false
        | _ -> true
      do
        _I body
      done
    | OblivIfCmd { test; thn; els } ->
      let v = eval ctxt test in
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
      let v = eval ctxt exp in
      let intro =
        match info with
        | Some s -> s ^ ": "
        | None -> "" in
      if mode = 1 then print_endline @@ intro ^ V.to_string v;
      in
  _I

let rec loop ctxt =
  begin
  match input_value ctxt.server.input with
  | M.Relay{sender;replyto;channel;bit;value;_} ->
    begin
      match H.find_opt ctxt.handlers channel with
      | Some {x;replych;decls;prelude;body} ->
        H.add ctxt.memory x value;
        ctxt.replyto <- Option.bind replyto (fun ch -> Some (sender,ch));
        Option.iter
          (fun (ch,Ty.ChType{reads;_}) ->
            let lvl = Ty.level reads in
            H.add ctxt.trust_map (Implicit ch) lvl) replych;
        let f (A.LocalDecl{x;init;_}) =
          H.add ctxt.memory x @@ eval ctxt init in
        List.iter f decls;
        begin match prelude with
        | Some prelude ->
          ctxt.mode <- [1;bit];
          interpCmd ctxt prelude;
          interpCmd ctxt (A.Cmd{cmd_base=A.PopCmd;pos=Lexing.dummy_pos});
        | None ->
          ctxt.mode <- [bit]
        end;
        interpCmd ctxt body;
        H.clear ctxt.memory;
        ctxt.replyto <- None;
        Option.iter
          (fun (ch,_) ->
            H.remove ctxt.trust_map (Implicit ch)) replych;
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
    ; mode = []
    ; replyto = None
    ; memory = H.create 1024
    ; store = H.create 1024
    ; handlers = H.create 1024
    ; trust_map = H.create 1024
    ; server = {input;output}
    } in
  let f (A.Ch{ch;x;replych;decls;prelude;body;_}) =
    H.add ctxt.handlers ch {x;replych;decls;prelude;body} in
  let g = function
    | (A.VarDecl{x;init;_}) ->
      let i = eval ctxt init in
      H.add ctxt.store x i
    | (A.InputDecl _) ->
      ()
    | (A.ChannelDecl{node;ch;chty=Ty.ChType{reads;_};_}) ->
      let lvl = Ty.level reads in
      H.add ctxt.trust_map (Explicit (node,ch)) lvl in
  
  List.iter f chs;
  List.iter g decls;

  output_value ctxt.server.output (M.Greet {sender=ctxt.name});
  flush ctxt.server.output;

  let _ = T.create (prompt ctxt) () in

  loop ctxt
