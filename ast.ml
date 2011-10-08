

(* Pour ne pas faire de confusions dans les types*)
type id = string

(* opérateurs employés dans les expressions arithmétiques *)
type operator = Plus | Minus | Times | Div | Mod
(* expressions arithmétiques : les cas de base peuvent être des variables ou des litéraux *)
type intExpr = Var of id | Int of int | Operation of Operator * IntExpr * IntExpr

(* un intervalle : employé lorsque l'on sélectionne une partie d'un fil épais *)
type range = { min : intExpr ; max : intExpr }



(* un fil en tant que rvalue *)
type wire_rvalue = id * int

(* 
Un fil en tant que lvalue. 
On distingue :
   - un fil qu a été passsé en input
   - un fil sortant d'un block déclaré localement
   - une partie d'un fil épais
   - la fusion de plusieurs fils pour obtenir un fil épais
*)
type wire_lvalue = 
    SimpleWire of id 
  | BlockWire of id * id  
  | SliceWire of wire_lvalue * range 
  | MergeWire of wire_lvalue list

(* 
Une instruction décrivant le corps d'un block.
Elle est composée :
   - d'un identifiant indiquant le block instancié
   - d'un identifiant auquel est lié le block créé
   - d'une liste de fils passés au block lors de la création
*)
type instruction = id * id * wire_lvalue list

(* une sortie est un couple composé d'une étiquette et d'un fil *)
type output = id * wire_lvalue

(*
Un block. Se décompose en :
  - un identifiant
  - des paramètres entiers sur lesquels faire une éventuelle récursion
  - des fils en entrée
  - des instructions composant son corps
  - des fils en sortie
*)
type block = {
  id : id ;
  params : intExpr list;
  inputs : wire_rvalue list;
  instructions : instruction list;
  outputs : output list
}


(* 
Un circuit combinatoire : 
   - un identifiant correspondant au bloc global
   - une liste des blocs définis dans le code
*)
type circuit = id * block list
