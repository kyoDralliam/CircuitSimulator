open Ast
open Integer


(**
   type employé pour le pattern-matching sur les entiers :
   Affine_Pattern( a, s, b) --> a*s + b               a <> 0
   Double_Pattern( a, s1, s2, b) --> a*s1 + s2 + b    a <> 1  && a <> 0 && s1 <> s2 
   Constant_Pattern( a ) --> a
   où a, b dénotent des entiers
   s, s1 et s2 dénotent des variables
*)
type pattern =
    Affine_Pattern of int * id * int 
  | Double_Pattern of int * id * id * int
  | Constant_Pattern of int

(** exception levée lorsqu'un paramètre n'a pas la forme requise *)
exception Bad_Pattern_Parameter

(** vérifie qu'un paramètre extrait d'une liste de paramètres est clos *)
let is_parameter_closed x = function
  | Constant_Pattern _ -> true 
  | _ -> false

(** 
    Une combinaison affine représente une somme de variables
    pondérés par des coefficients entiers plus un terme constant
    que l'on représente par le couple ("", n).
    On suppose (et on maintiendra ce fait) que la liste est triée
    par ordre croissant par rapport à l'identifiant.
*)
type affine_combination = (id*int) list  

(** Somme de deux combinaisons affines *)
let rec add ?(acc=[]) = function 
  | [], l | l, [] -> List.rev_append acc l
  | ((x,nx)::xs),((y,ny)::ys) -> match compare x y with
      | -1 -> add ~acc:((x,nx)::acc) (xs,(y,ny)::ys)
      | 0 -> if nx + ny <> 0 
	then add ~acc:((x,nx+ny)::acc) (xs, ys)
	else add ~acc (xs, ys)
      | 1 -> add ~acc:((y,ny)::acc) ((x,nx)::xs,ys)
      | _ -> failwith "cas impossible"

(** Y-a-t il autre chose que le terme
    constant dans la combinaison affine ?
    oui => retourne la constante, non => -1
*)
let is_const = function
  | [] -> 0  
  | [ "", n ] -> n
  | _ -> (-1) 

(** transforme un integer (interprété comme un pattern)
    en une combinaison affine
*)
let rec to_combo x = 
  match x with
    | Int n -> if n <> 0 then [ "", n ] else []
    | Var s ->  [ s, 1 ]
    | Unary_Op (Neg, n) -> List.map (fun (s,x) -> (s,-x)) (to_combo n)
    | Binary_Op (bop, n1, n2) ->
	match bop with
	  | Minus -> to_combo (Binary_Op (Plus, n1, Unary_Op( Neg, n2)))
	  | Plus -> add (( to_combo n1 ), ( to_combo n2 ))
	  | Times ->  
	      let p1 = to_combo n1 in
	      let p2 = to_combo n2 in		
	      let i1 = is_const p1 in		   
		if i1 <> -1 && i1 <> 0
		then List.map (fun (s,x) -> (s,i1*x)) p2
		else let i2 = is_const p2 in
		  if i2 <> -1 && i2 <> 0
		  then  List.map (fun (s,x) -> (s,i2*x)) p1
		  else 
		    if i1 = 0 || i2 = 0
		    then [ ]
		    else raise Bad_Pattern_Parameter
	  | _ -> raise Bad_Pattern_Parameter

(** transforme un integer en un pattern en le transformant d'abord
    en une combinaison affine
*)
let to_pattern x = 
  let ac = to_combo x in 
  let const,ac' = try (List.assoc "" ac) , (List.remove_assoc "" ac) with Not_found -> 0, ac in
    match List.length ac' with
      | 0 -> Constant_Pattern const
      | 1 -> (match ac' with [ s , n ] -> Affine_Pattern (n,s,const) 
		| _ -> raise Bad_Pattern_Parameter)
      | 2 -> (match ac' with
		| [(s1,n1);(s2,n2)] -> 
		    if n1 = 1 && n2 <> 1 then Double_Pattern(n2,s2,s1,const)
		    else if n2 = 1 && n1 <> 1 then Double_Pattern(n1,s1,s2,const)
		    else raise Bad_Pattern_Parameter
		| _ -> raise Bad_Pattern_Parameter)
      | _ -> raise Bad_Pattern_Parameter




