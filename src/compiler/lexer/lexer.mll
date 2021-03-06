{
  open Parser  
  exception Error of string
  let error lexbuf msg =
    let position = Lexing.lexeme_start_p lexbuf in
    let err_str = Printf.sprintf "Lexing error in file %s at position %d:%d\n"
                  position.pos_fname position.pos_lnum (position.pos_cnum - position.pos_bol + 1)
                  ^ msg ^ "\n" in
    raise (Error err_str)
}

let letter=['a'-'z' 'A'-'Z']
let digit=['0'-'9']
let idchar=letter | digit | '_'

rule token = parse
| [' ' '\t']          { token lexbuf }     (* skip blanks *)
| '\n'                { Lexing.new_line lexbuf; token lexbuf }
| eof                 { EOF }
| ','                 { COMMA }
| ';'                 { SEMICOLON }
| '('                 { LPAREN }
| ')'                 { RPAREN }
| '{'                 { LBRACE }
| '}'                 { RBRACE }
| '['                 { LBRACK }
| ']'                 { RBRACK }
| '+'                 { PLUS }
| '-'                 { MINUS }
| '^'                 { CARET }
| "="                 { EQ }
| "!="                { NEQ }
| '<'                 { LT }
| '>'                 { GT }
| "<="                { LE }
| ">="                { GE }
| "&&"                { AND }
| "||"                { OR }
| '@'                 { AT }
| ":="                { ASSIGN }
| "if"                { IF }
| "obliv"             { OBLIV }
| "then"              { THEN }
| "else"              { ELSE }
| "send"              { SEND }
| "while"             { WHILE }
| "do"                { DO }
| "skip"              { SKIP }
| "sleep"             { SLEEP }
| "hn"                { HN }
| "/*"                { comment 0 lexbuf }
| digit+ as i         { match int_of_string_opt i with
                        | Some i' -> INT i'
                        | None -> error lexbuf "Integer too large" }
| digit+ idchar+ as l { error lexbuf @@ "Invalid literal '" ^ l ^ "'" }
| "_" idchar+         { error lexbuf "Invalid identifier, id cannot start with '_'" }
| letter idchar* as id  { ID (id) }
| '\"'                { let start_p = lexbuf.lex_start_p in
                        let str = string "" lexbuf in
                        lexbuf.lex_start_p <- start_p;
                        STRING str}
| _ as t              { error lexbuf @@ "Invalid token '" ^ (String.make 1 t) ^ "'" }

and string current = parse
| '\"'                { current }
| '\n'                { error lexbuf "Illegal newline in string" }
| '\\' ['\\' '\"' 'n' 't'] as e
                      { string (current^(Scanf.unescaped e)) lexbuf }
| [' '-'~']           { string (current^(Lexing.lexeme lexbuf)) lexbuf }
| eof                 { error lexbuf "Unclosed string at end of file" }
| _ as c              { error lexbuf @@ "Illegal character '" ^ (String.make 1 c) ^ "'" }

and comment commentLevel = parse
| "/*"     { comment (commentLevel + 1) lexbuf }
| "*/"     { (if commentLevel = 0 then token else comment (commentLevel - 1)) lexbuf }
| '\n'     { Lexing.new_line lexbuf; comment commentLevel lexbuf }
| eof      { error lexbuf "Unclosed comment at end of file" }
| _        { comment commentLevel lexbuf }     (* continune *)
