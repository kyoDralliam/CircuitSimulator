open Ast

open IntegerToInt
open Pattern

(*

  vérifier la taille des fils pour ne pas en avoir des vides ==> OK
  (géré dans IntegerToInt)

  mettre en place des exceptions efficaces pour tracer les erreurs
*)



(** Map ayant comme clés des block_type et 
    comme valeur des block_type_definition *)
module ConcreteBlockMap = Map.Make(
  struct 
    type t = IntAst.block_type 
    let compare = compare 
  end)


(** vrai ssi tous les éléments de 
    la liste vérifient le prédicat p *)
let for_all p = List.fold_left (fun x y -> x && p y) true

let key_of_block block =
  let parameters = IntegerToInt.integer_list 
    StringMap.empty block.IntegerAst.parameters in
  block.IntegerAst.name, parameters 

(** vérifie qu'un integer est clos *)
let rec is_integer_closed x =
    let open Integer in
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

exception Failed_unification

let apply_pattern m p n =
  match p with
    | Constant_Pattern n' -> m 
    | Affine_Pattern (a,n',b) -> 
	if StringMap.mem n' m
	then failwith "deux variables ayant le même nom sont présents dans des pattern différents"
	else
	  if (n - b) mod a = 0 
	  then StringMap.add n' ((n-b)/a) m
	  else raise Failed_unification
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
    let pattern_list = List.map to_pattern block.IntegerAst.parameters in
      if for_all (is_parameter_closed StringMap.empty) pattern_list 
      then with_params,block::without_params
      else block::with_params, without_params
  in List.fold_left split ([],[])



(** vérifie qu'un block donné ne contient aucun paramètre libre 
    en supposant que les parameters ont déja été vérifiés *)
let check_and_add block_definitions block =
    let int_block = 
      try IntegerToInt.block_type_definition StringMap.empty block
      with Free_Variable s -> raise (Free_Variable ("block : "^block.IntegerAst.name ^ ", " ^s))
	| Zero_Sized_Wire s -> raise (Zero_Sized_Wire ("block : "^block.IntegerAst.name ^ ", " ^s))
    in
      ConcreteBlockMap.add (key_of_block block) int_block block_definitions 

(** Prend en arguments une liste d'int (parameters) et une liste 
    associative pattern list * IntegerAst.block_type_definition
*)
let get_block_type_definition parameters patterns_list = 
  let aux acc (patterns, block_def) =
    match acc with
      | Some x -> acc
      | None ->
	  try 
	    let subst = List.fold_left2 apply_pattern StringMap.empty patterns parameters in
	      Some (block_def, subst)
	  with Failed_unification | Invalid_argument _ -> None
  in match List.fold_left aux None patterns_list with
    | Some x -> x
    | None -> failwith "aucun pattern ne convient"

(** parcourt récursivement (en descendant) les blocs 
    et réifie (applique les paramètres en tant que valeurs)
    les blocs qui n'existent pas encore
    block_type : IntAst.block_type
    abstract_blocks : (pattern list, IntegerAst.block_definition) list StringMap.t
    concrete_blocks : IntAst.block_definition ConcreteBlockMap.t
*)
let rec reify_blocks block_type abstract_blocks concrete_blocks =
  if ConcreteBlockMap.mem block_type concrete_blocks
  then concrete_blocks
  else
    let id = fst block_type in
    let patterns = StringMap.find id abstract_blocks in
    let parameters = snd block_type in
      if not (for_all ((<=) 0) parameters)
      then failwith "récursion mal fondée paramètres négatifs trouvés" ;
      let block_def,map = get_block_type_definition parameters patterns in
      let int_block_def = IntegerToInt.block_type_definition map block_def in
      let next_call map x = reify_blocks x.IntAst.block_type abstract_blocks map 
      in ConcreteBlockMap.add block_type int_block_def
	   (List.fold_left next_call concrete_blocks int_block_def.IntAst.instantiations)
	
      (* 
	 vérifier que le block obtenu en substituant est bien défini,    ==> OK
	 créer récursivement les blocks pas encore connus,               ==> OK
	 vérifier que tous les nombres substitués sont positifs ou nuls  ==> OK
      *)
let add_abstract_block map block =
  let pattern_list = List.map to_pattern block.IntegerAst.parameters in
  let other_patterns = 
    try StringMap.find block.IntegerAst.name map
    with Not_found -> [] in
    StringMap.add block.IntegerAst.name ((pattern_list, block)::other_patterns) map
    

(** point d'entrée de l'analyseur sémantique *)
let analyse_circuit circuit = 
  let with_params, without_params = 
    split_if_closed_params (snd circuit) in
  let concrete_blocks = 
    List.fold_left check_and_add ConcreteBlockMap.empty without_params in
  let abstract_blocks = List.fold_left add_abstract_block StringMap.empty with_params in
    (fst circuit), (reify_blocks (fst circuit) abstract_blocks concrete_blocks)



