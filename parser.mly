%{
(** 
Premier jet de la syntaxe du langage donné au simulateur de circuit :

Program := start NomBloc

Bloc := 
     NomBloc < LIST1( Entier, ",") > (Entrées)
     Instructions
     -> Sorties ;

NomBloc := 
	IdentifiantMajuscule 

Entier := 
       0 | 1 | .. | n 

Entrées := LIST1( FilEntrée , "," ) 

FilEntrée := Identifiant | Identifiant[ IntExpr ] 

Fil := Identifiant | Fil[ RangeExpr ] | IdentifiantMajuscule.Identifiant | { LIST1( Fil, "," ) } 

RangeExpr := IntExpr | IntExpr ".." IntExpr 
 
IntExpr := Entier 
	| ( IntExpr ) 
	| IntExpr + IntExpr 
	| IntExpr - IntExpr 
	| IntExpr * IntExpr
	| IntExpr / IntExpr 
	| IntExpr % IntExpr 

Instructions := LIST1( Instruction , "")

Instruction := NomBloc IdentifiantMajuscule ( Paramètres ) 

Paramètres := LIST0( Fil, ",") 

Sorties := LIST1( SortieNomméeElementaire, "," ) 
SortieElementaire := Etiquette : Fil


*) 


(**
Exemple de code : 


Additionneur <1> (a, b) 
    DemiAdditionneur F (a, b)
    -> result[0] : F.s, c : F.carry ; 

Additionneur <n> (a[n] , b[n])
    Additionneur<n-1> A (a[0..n-2], b[0..n-2])  
    FullAdder F (a[n-1], b[n-1], A.c) 
    -> 
       result : { A.result, F.sortie, F.carry } , 
       c : F.carry ; 

*)


(**

Tokens à définir pour le lexer :

identifiantMajuscule     UID
identifiantminuscule     LID
start                    START
<                        LESS
>                        GREATER
(                        LPAREN
)                        RPAREN
->                       ARROW
;                        SEMI
:                        COLON
entier                   INT
,                        COMMA
[                        LSQBR
]                        RSQBR
{                        LBRACK
}                        RBRACK
.                        DOT
..                       DOTDOT
+                        PLUS
-                        MINUS
*                        TIMES
/                        DIV
%                        MOD
end_of_file              EOF

*)



  (* zone ocaml *)
  open Ast
%}

/* définition des tokens */
%token<string> UID LID
%token<int> INT 
%token LESS GREATER LPAREN RPAREN ARROW
%token SEMI COMMA LSQBR RSQBR START
%token LBRACK RBRACK DOT DOTDOT EOF
%token PLUS MINUS TIMES DIV MOD COLON

/* mise en place des priorités et des associativités */

%left PLUS MINUS
%left TIMES DIV MOD


%start circuit
%type <Ast.circuit> circuit

%%

/** Tools **/
%inline slist(S, x)        : l=separated_list(S, x)                    {l}
%inline snlist(S, x)       : l=separated_nonempty_list(S, x)           {l}
%inline beslist(B, E, S, x): B l=separated_list(S, x) E                {l}

/** corps du parser **/

circuit:
  | d=definition c=circuit     { fst c, d::(snd c) }
  | START n=LID c=circuit      { if fst c <> ""
				 then failwith "un seul bloc peut être marqué start"
				 else n, snd c }
  | EOF                        { "", [] }



%inline definition:
  | n=UID p=parameters inp=inputs ins=instanciations o=output 
    { { name = n ; parameters = p ; inputs = inp ; instantiations = ins ; outputs = o } }

%inline parameters:
  | l=loption( beslist(LESS, GREATER, COMMA, parameter) ) { l }

parameter:
  | n=INT { Parameter_Value n }
  | n=LID { Parameter_Name n }

%inline inputs:
  | LPAREN l=slist( COMMA, wire_declaration ) RPAREN { l }

wire_declaration:
  | n=LID                        { n, Int 1 }
  | n=LID LSQBR i=integer RSQBR  { n, i }

integer:
  | n=INT                              { Int n }
  | n=LID                              { Var n }
  | n1=integer op=binary_op n2=integer { Binary_Op (op,n1,n2) }
  | op=unary_op n=integer              { Unary_Op (op, n) }

%inline binary_op:
  | PLUS  { Plus }
  | MINUS { Minus }
  | TIMES { Times }
  | DIV   { Div }
  | MOD   { Mod }

unary_op:
  | MINUS { Neg }

%inline instanciations:
  | l=list( instanciation ) { l }

%inline instanciation:
  | b=block_type n=UID LPAREN ws=slist( COMMA, wire ) RPAREN
    { { block_type = b ; var_name = n ; input = ws } }

%inline block_type:
  | n=UID ps=loption( beslist(LESS, GREATER, COMMA, integer) )
    { n, ps }

wire:
  | wi=wire_identifier                     { Named_Wire wi }
  | LBRACK ws=snlist( COMMA, wire ) RBRACK { Merge ws }
  | s=slice                                { Slice s }

%inline wire_identifier:
  | n1=option( UID ) DOT n2=LID { n1, n2 }

slice:
  | w=wire RSQBR m=integer LSQBR 
    { { wire = w ; min = m ; max = m } }
  | w=wire RSQBR m1=integer DOTDOT m2=integer LSQBR
    { { wire = w ; min = m1 ; max = m2 } }

%inline output:
  | ARROW l=slist( COMMA, wire_definition) SEMI { l }

%inline wire_definition:
  | wd=wire_declaration COLON w=wire { wd, w } 


%%
