open Ast

open IntegerToInt
open Pattern
open BaseBlocks


(** vrai ssi tous les éléments de 
    la liste vérifient le prédicat p *)
let for_all p = List.fold_left (fun x y -> x && p y) true


(** Associe un IntAst.block_type à
    un IntegerAst.block_type_definition *)
let key_of_block block =
  let parameters = IntegerToInt.integer_list 
    StringMap.empty block.IntegerAst.parameters in
  block.IntegerAst.name, parameters 


(** exception levée lorsqu'un pattern ne peut 
    pas s'appliquer sur l'argument en entrée *)

exception Failed_unification

(** exception levée lorsque 2 pattern de la
    même pattern list assigne des valeurs à 
    la même variable 
*)
exception Same_name of string

(** Applique le pattern p à l'entier (int) n
    avec les variables déja liées dans la map
    m ( string -> int ) et affecte les variables
    dont on a éventuellement inférées les valeurs
    retourne la map m obtenue ou lève l'exception
    Failed_unification
*)
let apply_pattern m p n =
  match p with
    | Constant_Pattern n' -> if n = n' then m else raise Failed_unification
    | Affine_Pattern (a,n',b) -> 
	if StringMap.mem n' m
	then raise (Same_name n')
	else
	  if (n - b) mod a = 0 
	  then StringMap.add n' ((n-b)/a) m
	  else raise Failed_unification
    | Double_Pattern (a,n1,n2,b) ->
	if StringMap.mem n1 m 
	then raise (Same_name n1)
	else 
	  if StringMap.mem n2 m
	  then raise (Same_name n2)
	  else
	    let n1_val = (n - b) / a in
	    let n2_val = (n - b) mod a in
	      StringMap.add n1 n1_val
		(StringMap.add n2 n2_val m)


exception Instance_not_found of IntAst.block_type

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
    | None -> raise (Instance_not_found ("",[]))


exception Bad_recursion of IntAst.block_type list * string

(** parcourt récursivement (en descendant) les blocs 
    et réifie (applique les paramètres en tant que valeurs)
    les blocs qui n'existent pas encore
    block_type : IntAst.block_type
    abstract_blocks : (pattern list, IntegerAst.block_definition) list StringMap.t
    concrete_blocks : IntAst.block_definition ConcreteBlockMap.t
*)
let rec reify_blocks block_type abstract_blocks encountered_blocks concrete_blocks devices_names =
  if ConcreteBlockMap.mem block_type concrete_blocks
  then concrete_blocks
  else
    let id = fst block_type in
      if List.mem id devices_names
      then concrete_blocks
      else
	let patterns = 
	  try StringMap.find id abstract_blocks 
	  with Not_found -> raise (Instance_not_found block_type) in
	let parameters = snd block_type in
	  if not (for_all ((<=) 0) parameters)
	  then raise (Bad_recursion( [ block_type ], "paramètre(s) négatif(s)" )) ;
	  let block_def,map = 
	    try 
	      get_block_type_definition parameters patterns 
	    with Instance_not_found _ -> raise (Instance_not_found block_type)
	  in
	  let int_block_def = IntegerToInt.block_type_definition map block_def in
	  let new_encountered_blocks = BlockTypeSet.add block_type encountered_blocks in
	  let next_call map (x : IntAst.instantiation) = 
	    let x_block = x.IntAst.block_type in
	      if BlockTypeSet.mem x_block new_encountered_blocks 
	      then raise (Bad_recursion( [ block_type ; x_block ], "block déja rencontré" ))
	      else 
		if fst x_block = id && (snd x_block) >= parameters
		then raise (Bad_recursion ( [ block_type ; x_block ], "paramètres non décroissant" ))
		else 
		  try
		    reify_blocks x.IntAst.block_type abstract_blocks new_encountered_blocks map devices_names
		  with Bad_recursion (l,s) -> raise (Bad_recursion (block_type::l,s))
	  in ConcreteBlockMap.add block_type int_block_def
	       (List.fold_left next_call concrete_blocks int_block_def.IntAst.instantiations)


(** levée lorsque deux blocks du même nom
    comportent un nombre différent de pattern
*)
exception Bad_pattern_number of IntAst.block_type

(** ajoute le block à la map
    vérifie que le nombre de paramètres 
    est cohérent avec les autres block 
    du même nom (mais avec d'autres paramètres)
*)
let add_abstract_block map block =
  let pattern_list = List.map to_pattern block.IntegerAst.parameters in 
  let other_patterns = 
    try 
      let tmp = StringMap.find block.IntegerAst.name map in
	if List.length (fst (List.hd tmp)) <> List.length pattern_list
	then raise (Bad_pattern_number (key_of_block block))
	else tmp
    with Not_found -> [] in
    StringMap.add block.IntegerAst.name ((pattern_list, block)::other_patterns) map 


include Wire

exception Number_of_arguments of IntAst.instantiation

exception Bad_sized_wire of IntAst.wire

exception Bad_block_definition of string * int list * exn
exception Bad_size_enable of (IntAst.block_type) * string


(** Vérifie que toutes les variables (fils) employées 
    dans un block sont définies et que la taille des fils 
    corresponds
    à ajouter => émet un warning pour les fils non utilisés
*)
let check_variables block block_definitions devices = 
  let open IntAst in
  let open List in
  let open Wire in
    
  let variables = get_wire_identifier_map_size block block_definitions devices in
  
  let check ?except w wd = 
    if ( wire_size variables ?except w ) <> (snd wd ) 
    then raise (Bad_sized_wire w)
  in
    
  let check_device_instanciation inst =
    let open List in
    try 
      let i = assoc (fst inst.block_type) devices in
	if i <> length (snd inst.block_type) then failwith "" ;
	for_all (fun (x,y) -> x = y) 
	  (combine (map (wire_size variables) inst.input) 
	     [32;32;4;1;1;1])
    with 
	Not_found -> failwith ("Cas impossible -> tous les blocks " ^ 
	  "concrets et les devices ont aytéèss checkés" )
      | Invalid_argument _ -> failwith ""
  in

  let check_instantiation inst =
    try 
      let block = ConcreteBlockMap.find inst.block_type block_definitions in
	begin
	  match inst.enable with
	    | None -> ()
	    | Some w -> 
		if wire_size variables w <> 1 
		then
		  let excep = Bad_size_enable (inst.block_type,inst.var_name) in
		  raise (Bad_block_definition (block.name, block.parameters, excep))
	end;
	if List.mem block block_without_loop
	then iter2 (check ~except:inst.var_name) inst.input block.inputs
	else iter2 check inst.input block.inputs
    with 
	Not_found -> ignore (check_device_instanciation inst)
      | Invalid_argument _ -> raise (Number_of_arguments inst)
  in
  let check_output (wd,w) = check w wd in
    try
      iter check_instantiation block.instantiations ;
      iter check_output block.outputs
    with e -> raise (Bad_block_definition ( block.name, block.parameters, e))




(** vérifie les variables de tous les blocks concrets *)

let check_variables_in_blocks final_blocks devices = 
  let iter_check_variables k x = 
    if not (List.mem x base_block)
    then check_variables x final_blocks devices
  in
    ConcreteBlockMap.iter iter_check_variables final_blocks ;








exception Device_same_name of string

let check_devices_names block_defs devices_names =
  let check_with_block_def name = 
    if StringMap.mem name block_defs then raise (Device_same_name name)
  in
  let base_blocks_names = List.map (fun x -> x.IntAst.name) base_block in
  let check_with_base_block name =
    if List.mem name base_blocks_names then raise (Device_same_name name)
  in
    List.iter check_with_block_def devices_names ;
    List.iter check_with_base_block devices_names


(** Ajoute le block de base nommé s
    à la map map. Le block ajouté
    est un block "fantôme" sans 
    signification ni contenu
*)
let add_block map s =  
    ConcreteBlockMap.add (s.IntAst.name, []) s map

exception Undefined_start_block

(** point d'entrée de l'analyseur sémantique 
    retourne une paire formée d'un IntAst.block_type
    et d'une IntAst.block_type_definition ConcreteBlockMap.t
*)
let analyse_circuit (fst_circuit, circuit_blocks, circuit_devices) = 
  if fst_circuit = ("",[]) then raise Undefined_start_block ;
  let circuit_start = IntegerToInt.block_type StringMap.empty fst_circuit in
  let concrete_blocks = List.fold_left add_block ConcreteBlockMap.empty base_block in
  let devices_names = List.map fst circuit_devices in
  let abstract_blocks = List.fold_left add_abstract_block StringMap.empty circuit_blocks in
    check_devices_names abstract_blocks devices_names ;
    let final_blocks = reify_blocks circuit_start abstract_blocks 
      BlockTypeSet.empty concrete_blocks devices_names in
      check_variables_in_blocks final_blocks circuit_devices;
      circuit_start, final_blocks, (circuit_devices : IntAst.device_type_definition list)



    
    
