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
	  | Minus | Div | Mod -> raise Bad_Pattern_Parameter
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

(** transforme un integer en un pattern en le transformant d'abord
    en une combinaison affine
*)
let to_pattern x = 
  let ac = to_combo x in
  let const,ac' = try (List.assoc "" ac) , (List.remove_assoc "" ac) with Not_found -> 0, ac in
    match List.length ac' with
      | 0 -> Constant_Pattern const
      | 1 -> (match ac with [ s , n ] -> Affine_Pattern (n,s,const) 
		| _ -> raise Bad_Pattern_Parameter)
      | 2 -> (match ac with
		| [(s1,n1);(s2,n2)] -> 
		    if n1 = 1 && n2 <> 1 then Double_Pattern(n2,s2,s1,const)
		    else if n2 = 1 && n1 <> 1 then Double_Pattern(n1,s1,s2,const)
		    else raise Bad_Pattern_Parameter
		| _ -> raise Bad_Pattern_Parameter)
      | _ -> raise Bad_Pattern_Parameter













(** ATTENTION : Code dangeureux 
let get_block_type_definition parameters patterns_list = 
  let map = ref StringMap.empty in 
  let block_definition = ref IntegerAst.(
    { name = "" ; parameters = []; inputs = [] ; 
      instantiations = [] ; outputs = [] }) in
  let aux2 b param (pattern,block_def) = b && 
    let b,m = apply_pattern !map pattern param in 
      block_definition := block_def ;
      map := m ; 
      b
  in
  let aux b patterns = b && (* le && étant paresseux la map ne sera pas remise à 0 si un pattern est trouvé *)
    (map := StringMap.empty ;
     List.fold_left2 aux2 true parameters pattern) 
  in
    if List.fold_left aux true patterns_list
    then !block_definition, !map
    else failwith "Aucun pattern ne convient"*)


(** CODE MORT : ne reste que pour la postérité...
   transforme un paramètre (integer) en pattern

let rec check_parameter x = 
  let open Integer in
    match x with
      | Int n -> Constant_Pattern n
      | Var s -> Affine_Pattern(1,s,0)
      | Binary_Op (bop, n1, n2) ->
	  (match bop with
	    | Minus | Div | Mod -> raise Bad_Pattern_Parameter
	    | Plus ->
		let p1 = check_parameter n1 in
		let p2 = check_parameter n2 in
		  (match p1,p2 with
		    | (Constant_Pattern i1), (Constant_Pattern i2) ->
			Constant_Pattern (i1+i2)
		    | (Constant_Pattern i), (Affine_Pattern (j1,s,j2)) |
			  (Affine_Pattern (j1,s,j2)), (Constant_Pattern i) ->
			Affine_Pattern (j1,s,j2+i)
		    | (Constant_Pattern i), (Double_Pattern (j1,s1,s2,j2)) |
			  (Double_Pattern (j1,s1,s2,j2)), (Constant_Pattern i) ->
			Double_Pattern (j1,s1,s2,j2+i)
		    | (Affine_Pattern (j1,s1,j2)), (Affine_Pattern (i1,s2,i2)) ->
			if s1 = s2 
			then 
			  if j1+i1 = 0 
			  then Constant_Pattern (j2 + i2)
			  else Affine_Pattern (j1+i1,s1,j2+i2)
			else 
			  if j1 = 1 && i1 <> 1
			  then Double_Pattern (i1,s2,s1,j2+i2)
			  else 
			    if i1 = 1 && j1 <> 1
			    then Double_Pattern (j1,s1,s2,j2+i2)
			    else raise Bad_Pattern_Parameter
		    |  (Affine_Pattern (j1,s1,j2)), (Double_Pattern(i1,s2,s3,i2)) |
			   (Double_Pattern(i1,s2,s3,i2)), (Affine_Pattern (j1,s1,j2)) ->
			 if s1 = s2 && i1+j1 <> 0 && i1+j1 <> 1
			 then Double_Pattern(i1+j1, s1, s3, j2+i2)
			 else 
			   if s1 = s3
			   then 
			     if j1 = -1
			     then Affine_Pattern (i1,s2,j2+i2)
			     else raise Bad_Pattern_Parameter
		    | (Double_Pattern(i1,s1,s2,i2)), (Double_Pattern(j1,s3,s4,j2)) ->
			if s1 = s4 && i1 = -1
			then 
			  if s3 <> s2
			  then Double_Pattern(j1,s3,s2,i2+j2)
			  else 
			    if j1 = -1
			    then Constant_Pattern(i2+j2)
			    else Affine_Pattern(j1+1,s3,i2+j2)
			else 
			  if s3 = s2 && j1 = -1
			  then Double_Pattern(i1,s1,s4,i2+j2)
			  else
			    if s1 = s3
			    then raise Bad_Pattern_Parameter)
	    | Times ->
		let p1 = check_parameter n1 in
		let p2 = check_parameter n2 in
		  (match p1,p2 with
		    | (Constant_Pattern i1), (Constant_Pattern i2) ->
			Constant_Pattern (i1*i2)
		    | (Constant_Pattern i), (Affine_Pattern (j1,s,j2)) |
			  (Affine_Pattern (j1,s,j2)), (Constant_Pattern i) ->
			if i = 0 
			then (Constant_Pattern 0)
			else Affine_Pattern (i*j1,s,i*j2)
		    | (Constant_Pattern i), (Double_Pattern (j1,s1,s2,j2)) |
			  (Double_Pattern (j1,s1,s2,j2)), (Constant_Pattern i) ->
			if i = 0 
			then (Constant_Pattern 0)
			else 
			  if i = 1
			  then Double_Pattern (j1,s1,s2,j2)
			  else raise Bad_Pattern_Parameter
		    | _ -> raise Bad_Pattern_Parameter))
      | Unary_Op(Neg, n) -> check_parameter (Binary_Op (Times, Int(-1), n)) *)
