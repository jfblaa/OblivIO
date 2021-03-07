type config = {files: string list}

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

let interp files =
  Interpreter.Interp.interp files
  
let withFlags {files} =
  let exitCode = ref 0 in
  
  begin 
    try
        List.map parse files
        |> interp
    with
      CompileError -> (exitCode := 1)
  end;
  exit (!exitCode)

open Cmdliner

let src_arg =
  let doc = "Source file $(docv)." in
  Arg.(value & pos_all non_dir_file [] & info [] ~docv:"FILE" ~doc)

let check files =
  let config = {files} in
  withFlags config

let main_t =
  Term.(const check $ src_arg)

let info =
  let doc = "OblivIO compiler." in
  Term.info "oblivioc" ~version:"v0.1" ~doc ~exits:Term.default_exits

let _ = 
  Term.exit @@ Term.eval (main_t,info)