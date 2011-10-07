(** 
Premier jet de la syntaxe du langage donné au simulateur de circuit :

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
