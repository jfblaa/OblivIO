module M = Common.Message
module H = Hashtbl

let checkJsonPath json_file = 
  if not (Sys.file_exists json_file) 
  then failwith @@ "cannot find json file " ^ json_file

exception ServerFatal of string
let lookup m x =
  match H.find_opt m x with
  | Some v -> v
  | None -> raise @@ ServerFatal ("lookup")

let start json_file = 
  checkJsonPath json_file;
  let json = Yojson.Basic.from_file json_file in
  let open Yojson.Basic.Util in
  let addr_str = "127.0.0.1" in (*json |> member "addr" |> to_string in*)
  let port = 3050 in (*json |> member "port" |> to_int in*)
  let inet_addr = Unix.inet_addr_of_string addr_str in
  let sockaddr = Unix.ADDR_INET (inet_addr,port) in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_KEEPALIVE true;
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.setsockopt sock Unix.SO_REUSEPORT true;

  Unix.bind sock sockaddr;

  Unix.listen sock 5;

  let advlevel = json 
    |> member "attacker" 
    |> to_list 
    |> List.map to_string 
    |> Common.Level.of_list in

  print_endline @@ String.concat ""
    [ "server running at "
    ; addr_str
    ; ":"
    ; Int.to_string port 
    ; "..."
    ];

  let granularity = json |> member "granularity" |> to_float in
  let logbuf = Buffer.create 8192 in

  let loglock = Mutex.create () in
  let log str = 
    Mutex.lock loglock;
    Buffer.add_string logbuf str;
    Mutex.unlock loglock in
  
  let rec logger () =
    Mutex.lock loglock;
    Buffer.output_buffer stdout logbuf;
    Buffer.clear logbuf;
    Mutex.unlock loglock;
    flush stdout;
    Thread.delay granularity;
    logger () in

  let routing_table = H.create 1024 in

  let conn_main s = 
    let in_channel = Unix.in_channel_of_descr s in
    let out_channel = Unix.out_channel_of_descr s in
    
    let rec loop () =
      begin
      match input_value in_channel with
      | M.Greet {sender} ->
        print_endline @@ sender ^ " connected...";
        H.add routing_table sender out_channel;
        let open Common.Value in
        let msg = M.Relay{sender="OBLIVIO";receiver="";channel="START";level=Common.Level.bottom;bit=1;value=IntVal 0} in
        output_value out_channel msg;
        flush out_channel;
      | M.Relay {receiver;_} as msg ->
        let msgstr = M.to_string_at_level msg advlevel ^ "\n" in
        log msgstr;
        let ch = lookup routing_table receiver in
        output_value ch msg;
        flush ch
      | _ -> 
        print_endline "unrecognised input...";
        ()
      end;
      loop () in
    loop () in
  
  let _ = Thread.create logger () in

  (* https://eighty-twenty.org/2012/01/07/simple-networking-with-ocaml *)
  let rec accept_loop sock =
    let (s, _) = Unix.accept sock in
    let _ = Thread.create conn_main s in
    accept_loop sock in
  accept_loop sock
