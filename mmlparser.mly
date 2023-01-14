%{

 open Lexing
 open Mml

%}
%token TRUE FALSE REC IN THEN ELSE NOT TYPE INT BOOL UNIT MUTABLE PLUS AND OR
%token EQUALS R_PAR R_BRACKET COLON POINT R_ARROW SEMI_COLON IF FUN
%token STAR MINUS DIVIDE MOD EQUALS_COMP LESS_THAN LESS_OR_EQUAL_THAN DIFFERENT
%token L_PAR L_ARROW L_BRACKET LET
%token <int> CST
%token <string> IDENT
%token EOF

(*
Never usefull precedence levels:
FUN LET REC BOOL INT MUTABLE NOT TYPE UNIT POINT COLON
*)
%left IN

%right SEMI_COLON

%nonassoc THEN
%nonassoc ELSE

%right L_ARROW
%left R_ARROW

%right OR
%right AND

%left EQUALS_COMP DIFFERENT LESS_THAN LESS_OR_EQUAL_THAN

%left MINUS
%left PLUS
%left STAR DIVIDE MOD

%left IDENT

%nonassoc L_PAR L_BRACKET
%nonassoc CST TRUE FALSE

%start program
%type <Mml.prog> program

%%

program:
| t=list(t=typ_def{t}) e=expression EOF { {types=t; code=e} }
;

typ_entry:
| b=option(MUTABLE) i=IDENT COLON t=typ SEMI_COLON { match b with |None -> (i,t,false) | _ -> (i, t, true)}

typ_def:
| TYPE i=IDENT EQUALS L_BRACKET t=nonempty_list(typ_entry) R_BRACKET {i,t}

typ:
| n=INT {TInt}
| b=BOOL {TBool}
| u=UNIT {TUnit}
| i=IDENT {TStrct(i)} 
| t1=typ R_ARROW t2=typ {TFun(t1, t2)}
| L_PAR t=typ R_PAR { t }

expression:
| e=simple_expression { e }
| op=unop e=simple_expression { Uop(op, e)}
| e1=expression op=binop e2=expression { Bop(op, e1, e2) }
| e1=expression e2=simple_expression { App(e1, e2)} 
| IF c=expression THEN e2=expression {If(c,e2, Unit)}
| IF c=expression THEN e1=expression ELSE e2=expression {If(c,e1,e2)}
| FUN L_PAR i=IDENT COLON t=typ R_PAR R_ARROW e=expression {Fun(i,t,e)}
| LET     i1=IDENT l=list(L_PAR; i=IDENT; COLON; t=typ; R_PAR {i, t})             EQUALS e1=expression IN e2=expression {let f =                             mk_fun l e1 in Let(i1,f           ,e2)}
| LET REC i1=IDENT l=list(L_PAR; i=IDENT; COLON; t=typ; R_PAR {i, t}) COLON t=typ EQUALS e1=expression IN e2=expression {let f = mk_fun_type l t in let f2 = mk_fun l e1 in Let(i1,Fix(i1,f,f2),e2)}
| s=simple_expression POINT i=IDENT L_ARROW e=expression {SetF(s,i,e)}
| e1=expression SEMI_COLON e2=expression {Seq(e1,e2)}
;

simple_expression:
| n=CST { Int(n) }
| TRUE { Bool(true) }
| FALSE { Bool(false) }
| L_PAR R_PAR { Unit }
| i=IDENT { Var(i) }
| s=simple_expression POINT i=IDENT {GetF(s,i)}
| L_BRACKET t=nonempty_list(i=IDENT; EQUALS ;e=expression ;SEMI_COLON {i,e}) R_BRACKET {Strct(t)}
| L_PAR e=expression R_PAR { e }
;

%inline unop:
| MINUS { Neg }
| NOT { Not }

%inline binop:
| PLUS { Add }
| STAR { Mul }
| MINUS { Sub }
| DIVIDE { Div }
| MOD { Mod }
| EQUALS_COMP { Eq }
| DIFFERENT { Neq }
| LESS_THAN { Lt }
| LESS_OR_EQUAL_THAN { Le }
| AND { And }
| OR { Or }
;


