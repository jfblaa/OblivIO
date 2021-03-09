(**************************************************************************)
(* AU compilation.                                                        *)
(* Skeleton file -- expected to be modified as part of the assignment     *)
(* Do not distribute                                                      *)
(**************************************************************************)

%{
  open Common.Absyn
  open Common.Value
  module Level = Common.Level
%}

%token EOF
%token <string> ID
%token <int> INT
%token <string> STRING
%token DECLARE INIT
%token COMMA SEMICOLON
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token PLUS MINUS EQ NEQ LT LE GT GE CARET
%token AND OR ASSIGN IF THEN ELSE WHILE DO
%token SKIP HN OBLIV SEND AT PRINT
%token INTTYPE STRINGTYPE

%left OR
%left AND
%nonassoc EQ NEQ GT LT GE LE
%left PLUS MINUS 
%left CARET
%nonassoc UMINUS

%start <Common.Absyn.program> program  
(* Observe that we need to use fully qualified types for the start symbol *)

%%
%inline paren(X): LPAREN x=X RPAREN { x }
%inline brace(X): LBRACE x=X RBRACE { x }
%inline brack(X): LBRACK x=X RBRACK { x }
%inline slist(SEP,X): l=separated_list(SEP,X) { l }
%inline spair(X,SEP,Y): p=separated_pair(X,SEP,Y) { p }
%inline angled(X): LT x=X GT { x }

(* binop *)
%inline op:
| PLUS    { PlusOp }
| MINUS   { MinusOp }
| AND     { AndOp }
| OR      { OrOp }
| EQ      { EqOp }
| NEQ     { NeqOp }
| LT      { LtOp }
| LE      { LeOp }
| GT      { GtOp }
| GE      { GeOp }
| CARET   { ConcatOp }

binop_exp:
| MINUS right=exp %prec UMINUS
  { OpExp{left=Exp{exp_base=IntExp 0;pos=$startpos}; oper=MinusOp; right} }
| left=exp oper=op right=exp
  { OpExp{left; oper; right} }

lvl:
| ls=brace(slist(COMMA,ID))   { Level.of_list ls }

var:
| id=ID   { id }

(* Expressions *)
exp_base:
| i=INT       { IntExp i }
| s=STRING    { StringExp s }
| x=ID        { VarExp x }
| e=binop_exp { e }

exp:
| e=exp_base  { Exp {exp_base=e; pos=$startpos} }
| e=paren(exp_base)  { Exp {exp_base=e; pos=$startpos} }

cmd_base_seq:
| c=cmd_base
  { c }
| c1=cmd_base c2=cmd_seq
  { SeqCmd {c1=Cmd {cmd_base=c1; pos=$startpos};c2} }

cmd_base:
| v=var ASSIGN e=exp SEMICOLON
  { AssignCmd{var=v; exp=e} }
| SKIP SEMICOLON
  { SkipCmd }
| IF test=exp THEN thn=cmd ELSE els=cmd
  { IfCmd{test; thn; els} }
| OBLIV IF test=exp THEN thn=cmd ELSE els=cmd
  { OblivIfCmd{test; thn; els} }
| WHILE test=paren(exp) DO body=cmd
  { WhileCmd{test; body} }
| SEND LPAREN level=lvl COMMA tag=STRING COMMA exp=exp RPAREN SEMICOLON
  { SendCmd{level;tag;exp} }
| PRINT LPAREN info=ioption(terminated(STRING,COMMA)) exp=exp RPAREN SEMICOLON
  { PrintCmd{info;exp} }

cmd_seq:
| c=cmd_base_seq
  { Cmd {cmd_base=c; pos=$startpos} }

cmd:
| c=cmd_base
  { Cmd {cmd_base=c; pos=$startpos} }
| c=brace(cmd_base_seq)
  { Cmd {cmd_base=c; pos=$startpos} }

basevalue:
| i=INT
  { IntVal i }
| MINUS i=INT
  { IntVal (-i) }
| s=STRING
  { StringVal s }

type_anno:
| INTTYPE x=angled(ID) { x }
| STRINGTYPE x=angled(ID) { x }

vardecl:
| DECLARE sizevar=type_anno var=var ASSIGN basevalue=basevalue AT level=lvl SEMICOLON
  { VarDecl {var; basevalue; sizevar; level; pos=$startpos} }

hn:
| HN LPAREN level=lvl COMMA tag=STRING COMMA sizevar=type_anno var=var RPAREN cmd=cmd
  { Hn {level;tag;sizevar;var;cmd;pos=$startpos} }

init:
| INIT c=cmd { c }

(* Top-level *)
program:
| level=lvl vardecls=vardecl* init=init? hns=hn* EOF
  { Prog {level;vardecls;init;hns} }
