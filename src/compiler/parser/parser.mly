%{
  open Common.Absyn
  module Level = Common.Level
%}

%token EOF
%token <string> ID
%token <int> INT
%token <string> STRING
%token VAR INTERNAL REMOTE
%token SEMICOLON COMMA
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token PLUS MINUS EQ NEQ LT LE GT GE CARET
%token QMARK SIZE SLASH AT PRINT
%token AND OR ASSIGN BIND IF THEN ELSE WHILE DO
%token SKIP OBLIV SEND INPUT
%token INTTYPE STRINGTYPE

%left OR
%left AND
%nonassoc EQ NEQ GT LT GE LE
%left PLUS MINUS
%nonassoc UMINUS QMARK
%right CARET

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
| CARET   { CaretOp }

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
| i=INT             { IntExp i }
| s=STRING          { StringExp s }
| x=ID              { VarExp x }
| QMARK e=exp       { QuestionExp e }
| SIZE e=paren(exp) { SizeExp e }
| e=binop_exp       { e }

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
| OBLIV IF test=exp THEN thn=cmd ELSE els=cmd
  { OblivIfCmd{test; thn; els} }
| WHILE test=paren(exp) DO body=cmd
  { WhileCmd{test; body} }
| v=var ASSIGN INPUT RPAREN ch=ID COMMA size=exp RPAREN SEMICOLON
  { InputCmd {var=v;ch;size} }
| SEND LPAREN node=ID SLASH channel=ID COMMA exp=exp RPAREN SEMICOLON
  { SendCmd{node;channel;exp} }
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

type_anno:
| INTTYPE AT lvl=lvl            { IntType lvl }
| STRINGTYPE AT lvl=lvl         { StringType lvl }

decl:
| VAR ty=type_anno var=var ASSIGN init=exp padding=ioption(preceded(BIND,exp)) SEMICOLON
  { VarDecl {ty; var; init; padding; pos=$startpos} }
| INTERNAL ty=type_anno ch=ID SEMICOLON
  { InternalDecl {ty; ch; pos=$startpos} }
| REMOTE ty=type_anno node=ID SLASH ch=ID SEMICOLON
  { RemoteDecl {ty; node; ch; pos=$startpos} }

ch:
| ty=type_anno ch=ID var=paren(var) body=brace(cmd_seq)
  { Ch {ty;ch;var;body;pos=$startpos} }

(* Top-level *)
program:
| node=ID decls=decl* chs=ch* EOF
  { Prog {node;decls;chs} }
