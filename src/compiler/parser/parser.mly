%{
  open Common.Absyn
  module L = Common.Level
  module T = Common.Types
  module Ch = Common.Channel
%}

%token EOF
%token <string> ID
%token <int> INT
%token <string> STRING
%token VAR CHANNEL
%token SEMICOLON COMMA SEPARATOR DOT
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token PLUS MINUS TIMES DIVIDE EQ NEQ LT LE GT GE CARET
%token FST SND PUBLEN SECLEN
%token COLON SIZE AT PRINT EXIT PADTO
%token AND OR ASSIGN BIND IF THEN ELSE WHILE DO
%token SKIP OBLIF SEND INPUT
%token INTTYPE STRINGTYPE
%token REACH COST OVERHEAD

%left OR
%left AND
%nonassoc EQ NEQ GT LT GE LE
%left CARET PADTO
%left PLUS MINUS
%left TIMES DIVIDE
%right FST SND
%nonassoc UMINUS

%start <Common.Absyn.program> program  

%%
%inline paren(X): LPAREN x=X RPAREN { x }
%inline brace(X): LBRACE x=X RBRACE { x }
%inline brack(X): LBRACK x=X RBRACK { x }
%inline slist(SEP,X): l=separated_list(SEP,X) { l }
%inline spair(X,SEP,Y): p=separated_pair(X,SEP,Y) { p }

(* binop *)
%inline op:
| PLUS    { PlusOp }
| MINUS   { MinusOp }
| TIMES   { TimesOp }
| DIVIDE  { DivideOp }
| AND     { AndOp }
| OR      { OrOp }
| EQ      { EqOp }
| NEQ     { NeqOp }
| LT      { LtOp }
| LE      { LeOp }
| GT      { GtOp }
| GE      { GeOp }
| CARET   { CaretOp }
| PADTO   { PadOp }

binop_exp:
| MINUS right=exp %prec UMINUS
  { OpExp{left=Exp{exp_base=IntExp 0;pos=$startpos}; oper=MinusOp; right} }
| left=exp oper=op right=exp
  { OpExp{left; oper; right} }

lvl:
| ls=brace(slist(COMMA,ID))   { L.of_list ls }

var_base:
| x=ID { SimpleVar x}
| var=var exp=brack(exp)
  { SubscriptVar {var;exp} }

var:
| var_base=var_base { Var {var_base; pos=$startpos} }

(* Expressions *)
exp_base:
| i=INT             { IntExp i }
| s=STRING          { StringExp s }
| v=var             { VarExp v }
| SIZE e=paren(exp) { SizeExp e }
| e=binop_exp       { e }
| FST exp=exp       { ProjExp {proj=Fst; exp} }
| SND exp=exp       { ProjExp {proj=Snd; exp} }
| var=var DOT PUBLEN
  { LengthExp {public=true;var} }
| var=var DOT SECLEN
  { LengthExp {public=false;var} }
| pair=paren(spair(exp,COMMA,exp))
  { PairExp pair }
| arr=brack(separated_nonempty_list(SEMICOLON,exp))
  { ArrayExp arr }

exp:
| e=exp_base          { Exp {exp_base=e; pos=$startpos} }
| e=paren(exp_base)   { Exp {exp_base=e; pos=$startpos} }

cmd_base_seq:
| c=cmd_base
  { c }
| c1=cmd_base c2=cmd_seq
  { SeqCmd {c1=Cmd {cmd_base=c1; pos=$startpos};c2} }

cmd_base:
| v=var ASSIGN e=exp SEMICOLON
  { AssignCmd{var=v; exp=e} }
| v=var BIND e=exp SEMICOLON
  { BindCmd{var=v; exp=e} }
| SKIP SEMICOLON
  { SkipCmd }
| IF test=exp THEN thn=cmd ELSE els=cmd
  { IfCmd{test; thn; els} }
| OBLIF test=exp THEN thn=cmd ELSE els=cmd
  { OblivIfCmd{test; thn; els} }
| WHILE test=exp DO body=cmd
  { WhileCmd{test; body} }
| v=var BIND INPUT LPAREN size=exp RPAREN SEMICOLON
  { InputCmd {var=v;size} }
| SEND LPAREN channel=channel COMMA exp=exp RPAREN SEMICOLON
  { SendCmd{channel;exp} }
| PRINT LPAREN info=ioption(terminated(STRING,COMMA)) exp=exp RPAREN SEMICOLON
  { PrintCmd{info;exp} }
| EXIT LPAREN RPAREN SEMICOLON
  { ExitCmd }

cmd_seq:
| c=cmd_base_seq
  { Cmd {cmd_base=c; pos=$startpos} }

cmd:
| c=cmd_base
  { Cmd {cmd_base=c; pos=$startpos} }
| c=brace(cmd_base_seq)
  { Cmd {cmd_base=c; pos=$startpos} }

basetype:
| INTTYPE
  { T.INT }
| STRINGTYPE
  { T.STRING }
| LPAREN t1=type_at_lvl TIMES t2=type_at_lvl RPAREN
  { T.PAIR (t1,t2) }
| t=type_at_lvl LBRACK RBRACK
  { T.ARRAY t }

%inline type_at_lvl:
| base=basetype AT level=lvl  { T.Type{base;level} }

%inline type_anno:
| COLON t=type_at_lvl  { t }

channel:
| node=ID DIVIDE handler=ID
  { Ch.Ch {node;handler} }

effect:
| REACH channels=brack(separated_nonempty_list(SEMICOLON,channel))
  { Reach {channels;pos=$startpos} }
| COST cost=INT
  { Cost {cost;pos=$startpos} }
| OVERHEAD overhead=INT
  { Overhead {overhead;pos=$startpos} }

decl:
| VAR x=ID ty_opt=ioption(type_anno) ASSIGN init=exp SEMICOLON
  { VarDecl {ty_opt; x; init; pos=$startpos} }
| CHANNEL effects=effect* channel=channel AT level=lvl ty=type_anno SEMICOLON
  { ChannelDecl {ty; level; channel; effects; pos=$startpos(channel)} }
| INPUT AT level=lvl SEMICOLON
  { InputDecl {level; pos=$startpos} }

%inline localdecl:
| VAR x=ID ty_opt=ioption(type_anno) ASSIGN init=exp SEMICOLON
  { LocalDecl {ty_opt; x; init; pos=$startpos} }

%inline localdecls:
|                        { [] }
| l=localdecl+ SEPARATOR { l }

%inline prelude:
| cmd=cmd_seq SEPARATOR { cmd }

handler:
| effects=effect* handler=ID AT level=lvl LPAREN x=ID ty=type_anno RPAREN LBRACE decls=localdecls prelude=ioption(prelude) body=cmd_seq RBRACE
  { Hl {handler;effects;x;ty;level;decls;prelude;body;pos=$startpos(handler)} }

(* Top-level *)
program:
| node=ID decls=decl* hls=handler* EOF
  { Prog {node;decls;hls} }
