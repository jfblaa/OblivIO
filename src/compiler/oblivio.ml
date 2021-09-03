exception CompileError

(** Open the file and initialize the lexer buffer. Observe that the input 
    buffer must be closed by the caller. *)

let initLexer filename = 
  let input = open_in filename in
  let filebuf = Lexing.from_channel input in
  (* obs that we need to initialize the pos_fname field ourselves *)
  filebuf.lex_curr_p <- { filebuf.lex_curr_p with pos_fname = filename };  
  (input, filebuf)

let parse file = 
  let input, filebuf = initLexer file in 
  let parseRes = 
    try Parser.program Lexer.token filebuf
    with
    | Lexer.Error msg -> Printf.eprintf "%s%!" msg; raise CompileError
    | Parser.Error ->  
      let pos1 = Lexing.lexeme_start_p filebuf in
      let pos2 = Lexing.lexeme_end_p filebuf in
      let lexeme = Lexing.lexeme filebuf in
      Printf.fprintf stderr "%s:%d:%d - %d:%d: syntax error '%s'\n"
        pos1.pos_fname pos1.pos_lnum (pos1.pos_cnum - pos1.pos_bol)
        pos2.pos_lnum (pos2.pos_cnum - pos2.pos_bol + 1)
        lexeme;
      raise CompileError
  in 
  close_in input;
  parseRes

let semant prog =
  let success, res = Semant.transProg prog in
  if success
  then res
  else raise CompileError

let interp progs =
  Interpreter.Interp.interp progs
  
let client file =
  let exitCode = ref 0 in
  
  begin 
    try
      parse file
      |> semant
      |> interp
    with
      CompileError -> (exitCode := 1)
  end;
  exit (!exitCode)

open Cmdliner

let src_arg =
  let doc = "Source file $(docv)." in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"FILE" ~doc)

let server_arg =
  let doc = "Start OblivIO interpreter in $(docv) mode." in
  Arg.(value & flag & info ["s";"server"] ~docv:"SERVER" ~doc)

let check file is_server =
  if is_server
  then Interpreter.Server.start file
  else client file

let main_t =
  Term.(const check $ src_arg $ server_arg)

let info =
  let doc = "OblivIO interpreter." in
  Term.info "OblivIO" ~version:"v0.3" ~doc ~exits:Term.default_exits

let _ = 
  Term.exit @@ Term.eval (main_t,info)