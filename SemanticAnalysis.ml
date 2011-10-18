open Ast

include IntegerToInt
include Pattern
(*

vérifier la taille des fils pour ne pas en avoir des vides

*)



(** Map ayant comme clés des block_type et 
    comme valeur des block_type_definition *)
module ConcreteBlockMap = Map.Make(
  struct 
    type t = IntAst.block_type 
    let compare = compare 
  end)

(** opérateur de composition *)
let (&) f g x = f (g x)

(** vrai ssi tous les éléments de 
    la liste vérifient le prédicat p *)
let for_all p = List.fold_left (fun x y -> x && p y) true

let block_type_of_block_type_definition x =
  x.name, List.map (fun Parameter_Value y -> y | _ -> failwith "variable libre") x.parameters

(** vérifie qu'un integer est clos *)
let rec is_integer_closed x =
    let open IntegerAst in
      match x with
	| Int _ -> true
	| Var _ -> false
	| Binary_Op (_,l,r) -> 
	    (is_integer_closed l) && (is_integer_closed r)
	| Unary_Op (_,i) -> is_integer_closed i

(** vérifie qu'un wire est clos *)
let rec is_wire_closed x =
  let open IntegerAst in
    match x with
      | Named_Wire _ -> true
      | Merge l -> for_all is_wire_closed l
      | Slice s -> (is_integer_closed s.min) && (is_integer_closed s.max)

		let p1 = check_parameter n1 in
		let p2 = check_parameter n2 in
		let is_const = function
		   | [ "", n ] -> n
		   | _ -> -1 in
		let i1 = is_const p1 in		   
		  if i1 <> -1 
		  then List.iter (fun (s,x) -> (s,i1*x)) p2
		  else let i2 = is_const p2 in
		    if i2 <> -1 
		    then  List.iter (fun (s,x) -> (s,i2*x)) p1
		    else raise Bad_Pattern_Parameter




let apply_pattern m p n =
  match p with
    | Constant_pattern n' -> n = n',m 
    | Affine_Pattern (a,n',b) -> 
	if StringMap.mem n' m
	then failwith "deux variables ayant le même nom sont présents dans des pattern différents"
	else
	  if (n - b) mod a = 0 
	  then true, StringMap.add n' ((n-b)/a) m
	  else false, m
    | Double_Pattern (a,n1,n2,b) ->
	if StringMap.mem n1 m || StringMap.mem n2 m
	then failwith "deux variables ayant le même nom sont présents dans des pattern différents"
	else 
	  let n1_val = (n - b) / a in
	  let n2_val = (n - b) mod a in
	    StringMap.add n1 n1_val
	      (StringMap.add n2 n2_val m)

(** sépare une block_type_definition list en deux suivant que
    les block_type contiennent des paramétres libres *)
let split_if_closed_params =
  let split (with_params, without_params) block = 
    let pattern_list = List.map check_parameter block.parameters in
      if for_all is_parameter_closed pattern_list 
      then with_params,block::without_params
      else block::with_params, without_params
  in List.fold_left split ([],[])


(*
*
*   A REFAIRE !!!!!!!!
*
*)
(** vérifie qu'un block donné ne contient aucun paramètre libre 
    en supposant que les parameters ont déja été vérifiés *)
let check_and_add block_definitions block_without_params =
  let check_inputs = for_all (is_integer_closed & snd) in
  let check_instantiations block_inst =
    let check_integer_list = for_all is_integer_closed in
      for_all (fun x -> check_integer_list (snd x.block_type)) block_inst &&
	for_all (fun x -> for_all is_wire_closed x.input) block_inst
  in
  let check_outputs =
    for_all (fun (wd,w) -> is_integer_closed (snd wd) && is_wire_closed w)
  in
  let aux map block =
    if check_inputs block.inputs && 
      check_instantiations block.instantiations &&
      check_outputs block.outputs
    then let key = block_type_of_block_type_definition block in
      ConcreteBlockMap.add key block map 
  in
    List.fold_left aux block_definitions block_without_params

(** ATTENTION : Code dangeureux *)
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
    else failwith "Aucun pattern ne convient"

(** parcourt récursivement (en descendant) les blocs 
    et réifie (applique les paramètres en tant que valeurs)
    les blocs qui n'existent pas encore
    block_type : IntAst.block_type
    abstract_blocks : (pattern list, IntegerAst.block_definition) list StringMap.t
    concrete_blocks : IntAst.block_definition ConcreteBlockMap.t
*)
let rec reify_blocks block_type abstract_blocks concrete_blocks =
  if not ConcreteBlockMap.mem block_type concrete_blocks
  then 
    let id = fst block_type in
    let patterns = StringMap.find id abstract_blocks in
    let parameters = snd block_type in
      if not (for_all ((<=) 0) parameters)
      then failwith "récursion mal fondée paramètres négatifs trouvés" ;
      let block_def,map = get_block_type_definition parameters patterns in
      let int_block_def = IntegerToInt.block_type_definition map block_def in
      let next_call map x = reify_blocks (x.name,x.parameters) abstract_blocks map 
      in ConcreteBlockMap.add block_type int_block_def
	(List.fold_left next_call concrete_blocks int_block_def.instanciations)
	
      (* 
	 vérifier que le block obtenu en substituant est bien défini,    ==> OK
	 créer récursivement les blocks pas encore connus,               ==> OK
	 vérifier que tous les nombres substitués sont positifs ou nuls  ==> OK
      *)
let add_abstract_block map block =
  let pattern_list = List.map check_parameter block.parameters in
  let other_patterns = 
    try StringMap.find block.name map
    with Not_Found -> [] in
    StringMap.add block.name ((pattern_list, block)::other_patterns) map
    

(** point d'entrée de l'analyseur sémantique *)
let analyse_circuit circuit = 
  let with_params, without_params = 
    split_if_closed_params (snd circuit) in
  let concrete_blocks = 
    List.fold_left check_and_add ConcreteBlockMap.empty without_params in
  let abstract_blocks = List.fold_left add_abstract_block StringMap.empty with_params in
    (fst circuit), (reify_blocks (fst circuit) with_params concrete_blocks)



