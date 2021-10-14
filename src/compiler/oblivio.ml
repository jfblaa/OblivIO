exception CompileError

let initLexer filename = 
  let input = open_in filename in
  let filebuf = Lexing.from_channel input in
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

let interp prog =
  Interpreter.Interp.interp prog
  
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

let check file =
  if Filename.extension file |> String.equal ".json"
  then Interpreter.Server.start file
  else client file

let main_t =
  Term.(const check $ src_arg)

let info =
  let doc = "OblivIO interpreter." in
  Term.info "OblivIO" ~version:"v0.3" ~doc ~exits:Term.default_exits

let _ = 
  Term.exit @@ Term.eval (main_t,info)