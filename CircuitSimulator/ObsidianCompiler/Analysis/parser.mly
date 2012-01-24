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
  open Integer
  open IntegerAst

  type circuit_element = 
      Block of block_type_definition 
    | Start of block_type
    | Device of block_type

  let circuit_from_circuit_element_list l0 =
    let f (start,block_defs,devices) = function
      | Block b -> start,b::block_defs, devices
      | Start start' -> 
	  if start <> None 
	  then failwith "un seul bloc peut être marqué start"
	  else (Some start'), block_defs, devices
      | Device d ->
	  start, block_defs, (fst d, List.length (snd d))::devices
    in 
      List.fold_left f (None,[],[]) l0 

  let adapt_wire f w = f (fst w), snd w

  let adapt_wire_list f ws = 
    let l0, l1 = List.split ws in
    let must_add_vdd_gnd = List.exists (fun x -> x) l1 in
      f l0, must_add_vdd_gnd 

  let vdd = { block_type = "Vdd", [] ; enable = None ; var_name = "__Vdd" ; input = [] } 
  let gnd = { block_type = "Gnd", [] ; enable = None ; var_name = "__Gnd" ; input = [] }

  let add_vdd_gnd b l = 
    if b 
    then vdd::gnd::l
    else l

  let create_vdd_gnd_wires s =
    let l = ref [] in
    let create_wire = function
      | '0' -> l := (Named_Wire (Some "__Gnd", "o"))::!l
      | '1' -> l := (Named_Wire (Some "__Vdd", "o"))::!l
      | _ -> assert false
    in
      String.iter create_wire s ;
      Merge (List.rev !l) , true
%}

/* définition des tokens */
%token<string> UID LID CONST
%token<int> INT 
%token LESS GREATER LPAREN RPAREN ARROW
%token SEMI COMMA LSQBR RSQBR START
%token LBRACK RBRACK DOT DOTDOT EOF
%token PLUS MINUS TIMES DIV MOD COLON
%token POWER DEVICE AT

/* mise en place des priorités et des associativités */

%left PLUS MINUS
%left TIMES DIV MOD
%right POWER

%start circuit
%type <Ast.IntegerAst.circuit> circuit

/*********************
 *                   *
 *    DEBUG ONLY     *
 *                   *
 *********************/
%start integer_start
%type <Ast.Integer.integer> integer_start

%%

/** Tools **/
%inline slist(S, x)        : l=separated_list(S, x)                    {l}
%inline snlist(S, x)       : l=separated_nonempty_list(S, x)           {l}
%inline beslist(B, E, S, x): B l=separated_list(S, x) E                {l}


/*********************
 *                   *
 *    DEBUG ONLY     *
 *                   *
 *********************/

integer_start:
    | n=integer EOF { n }

/** corps du parser **/

circuit:
  | l=list( circuit_element ) EOF { circuit_from_circuit_element_list l }

circuit_element:
  | d=definition           { Block d }
  | START n=block_type     { Start n }
  | DEVICE n=block_type    { Device n }

definition:
  | n=UID p=parameters inp=inputs ins=instantiations o=output 
    { { name = n ; parameters = p ; inputs = inp ; 
	instantiations = add_vdd_gnd ((snd ins)||(snd o)) (fst ins) ; 
	outputs = fst o } }

parameters:
  | l=loption( beslist(LESS, GREATER, COMMA, integer) ) { l }

inputs:
  | l=loption( beslist( LPAREN, RPAREN, COMMA, wire_declaration ) ) { l }

wire_declaration:
  | n=LID                        { n, Int 1 }
  | n=LID LSQBR i=integer RSQBR  { n, i }

integer:
  | n1=integer op=binary_op n2=integer { Binary_Op (op,n1,n2) }
  | n=integer_atom                     { n }

integer_atom:
  | n=INT                              { Int n }
  | n=LID                              { Var n }
  | op=unary_op n=integer_atom         { Unary_Op (op, n) }
  | LPAREN n=integer RPAREN            { n }

%inline binary_op:
  | PLUS  { Plus }
  | MINUS { Minus }
  | TIMES { Times }
  | DIV   { Div }
  | MOD   { Mod }
  | POWER { Power }

unary_op:
  | MINUS { Neg }

instantiations:
  | l=list( instantiation ) 
    { adapt_wire_list (fun x -> x) l }

instantiation:
  | b=block_type w=enable?
    n=UID ws=loption( beslist( LPAREN, RPAREN, COMMA, wire ) )
    { 
      let w, b1 = match w with Some w -> Some (fst w), snd w | None -> None, false in 
      let ins, b2 = adapt_wire_list 
	(fun x -> { block_type = b ; enable = w; var_name = n ; input = x }) ws in
	ins, (b1||b2)
    }


enable:
  | AT w=wire { w }

block_type:
  | n=UID ps=loption( beslist(LESS, GREATER, COMMA, integer) )
    { n, ps }

wire:
  | wi=wire_identifier                     { Named_Wire wi, false }
  | LBRACK ws=snlist( COMMA, wire ) RBRACK { adapt_wire_list (fun ws -> Merge ws) ws }
  | s=slice                                { Slice s, false }
  | s=CONST                                { create_vdd_gnd_wires s }

%inline wire_identifier:
  | n1=UID DOT n2=LID { Some n1, n2 }
  | n=LID             { None , n }

slice:
  | w=wire_identifier LSQBR m=integer RSQBR 
    { { wire = w ; min = m ; max = m } }
  | w=wire_identifier LSQBR m1=integer DOTDOT m2=integer RSQBR
    { { wire = w ; min = m1 ; max = m2 } }

output:
  | l=beslist( ARROW, SEMI, COMMA, wire_definition) 
    { adapt_wire_list (fun x -> x) l }

%inline wire_definition:
  | wd=wire_declaration COLON w=wire { adapt_wire (fun x -> (wd, x)) w } 


%%





