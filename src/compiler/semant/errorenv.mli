type errorenv 
exception CompilerError of string 

val error: errorenv -> Lexing.position -> string -> unit 
val any_errors: errorenv -> bool 
val initial_env: errorenv
