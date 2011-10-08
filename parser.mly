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

*)


%{
  (* zone ocaml *)

%}

%token<string> UID LID
%token<int> INT
%token LESS GREATER LPAREN RPAREN ARROW
%token SEMI COMMA LSQBR RSQBR START
%token LBRACK RBRACK DOT DOTDOT
%token PLUS MINUS TIMES DIV MOD
