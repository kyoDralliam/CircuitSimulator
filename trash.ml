

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


(*
let rec check_acyclicity block blocks encountered_blocks =
  if not (List.mem (fst block) base_block)
  then 
    if BlockTypeSet.mem block encountered_blocks
    then failwith "block déja rencontré"
    else 
      let new_encnt_blocks = BlockTypeSet.add block encountered_blocks in
      let next x = check_acyclicity x.IntAst.block_type blocks new_encnt_blocks in
	List.iter next (ConcreteBlockMap.find *)

(** point d'entrée de l'analyseur sémantique *)
let analyse_circuit (circuit_start, circuit_params) = 
  let with_params, without_params = 
    split_if_closed_params circuit_params in
  let concrete_blocks = 
    List.fold_left check_and_add ConcreteBlockMap.empty without_params in
  let abstract_blocks = List.fold_left add_abstract_block StringMap.empty with_params in
  let final_blocks = reify_blocks circuit_start abstract_blocks BlockTypeSet.empty concrete_blocks in
    check_acyclicity start_block final_blocks BlockTypeSet.empty;
    (fst circuit), final_blocks
