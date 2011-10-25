open Ast

open IntegerToInt
open Pattern

(*

  vérifier la taille des fils pour ne pas en avoir des vides ==> OK
  (géré dans IntegerToInt)

  mettre en place des exceptions efficaces pour tracer les erreurs
*)

(* 
   vérifier que le block obtenu en substituant est bien défini,    ==> OK
   créer récursivement les blocks pas encore connus,               ==> OK
   vérifier que tous les nombres substitués sont positifs ou nuls  ==> OK
*)


module BlockType =
struct 
  type t = IntAst.block_type 
  let compare = compare 
end

(** Map ayant comme clés des block_type et 
    comme valeur des block_type_definition *)
module ConcreteBlockMap = Map.Make(BlockType)

(** Set ayant comme clés des block_type *)
module BlockTypeSet = Set.Make(BlockType)



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
let rec reify_blocks (block_type : IntAst.block_type)  abstract_blocks encountered_blocks concrete_blocks =
  if ConcreteBlockMap.mem block_type concrete_blocks
  then concrete_blocks
  else
    let id = fst block_type in
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
	  if fst x_block = id && compare (snd x_block) parameters <> -1
	  then raise (Bad_recursion ( [ block_type ; x_block ], "paramètres non décroissant" ))
	  else 
	    try
	      reify_blocks x.IntAst.block_type abstract_blocks new_encountered_blocks map 
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
    
(** Blocks de base : plutôt essayer d'aller 
    les chercher dans un autre fichier 
*)
let base_block = [ "Xor" ; "And" ; "Or" ; "Mux" ; "Reg" ; "Not" (* ... *)]



(** Employé pour vérifier que les variables 
    employées sont bien déclarées
*)
module WireIdentMap = Map.Make(
  struct
    type t = IntAst.wire_identifier
    let compare = compare
  end)

(** Vérifie que toutes les variables (fils) employées 
    dans un block sont définies et que la taille des fils 
    corresponds
    à ajouter => émet un warning pour les fils non utilisés
*)
let check_variables block block_definitions = 
  let open IntAst in
  let open List in
  let add_block_variables acc inst = 
    try 
      let block = ConcreteBlockMap.find inst.block_type block_definitions in
      let add acc ((s,i),_) = WireIdentMap.add  ((Some inst.var_name),s) i acc in 
	fold_left add acc block.outputs
    with Not_found -> acc
  in
  let variables = 
    let add_input_variables acc (x,i) =  WireIdentMap.add (None,x) i acc in
    let input_variables = fold_left add_input_variables WireIdentMap.empty block.inputs in
      fold_left add_block_variables input_variables block.instantiations
  in
  let rec wire_size ?except = function
    | Named_Wire wi -> 
	if (fst wi) <> except
	then WireIdentMap.find wi variables
	else failwith "boucle"
    | Merge l -> fold_left (+) 0 (map (wire_size ?except) l)
    | Slice s -> 
	let size_wi = WireIdentMap.find s.wire variables in
	  if s.min >= 0 && s.max < size_wi && s.max - s.min + 1 > 0
	  then s.max - s.min + 1
	  else failwith "slice incorrect"
  in
  let check ?except w wd = 
    if ( wire_size ?except w ) <> (snd wd ) 
    then failwith "fil pas de la bonne taille"
  in
  let check_instantiation inst =
    try 
      let block = ConcreteBlockMap.find inst.block_type block_definitions in
	if block.name <> "Reg" && List.mem block.name base_block
	then iter2 (check ~except:inst.var_name) inst.input block.inputs
	else iter2 check inst.input block.inputs
    with 
	Not_found -> failwith ""
      | Invalid_argument _ -> failwith "le nombre d'arguments passés à ce block n'est pas bon"
  in
  let check_output (wd,w) = check w wd in
    iter check_instantiation block.instantiations ;
    iter check_output block.outputs




(** Ajoute le block de base nommé s
    à la map map. Le block ajouté
    est un block "fantôme" sans 
    signification ni contenu
*)
let add_ghost map s =  
  let btdef = IntAst.({ name = s ; parameters = [] ; inputs = [] ; instantiations = [] ; outputs = [] }) in 
    ConcreteBlockMap.add (s, []) btdef map

(** point d'entrée de l'analyseur sémantique *)
let analyse_circuit circuit = 
  let circuit_start = IntegerToInt.block_type StringMap.empty (fst circuit) in
  let circuit_blocks = snd circuit in
  let concrete_blocks = List.fold_left add_ghost ConcreteBlockMap.empty base_block in
  let abstract_blocks = List.fold_left add_abstract_block StringMap.empty circuit_blocks in
  let final_blocks = reify_blocks circuit_start abstract_blocks BlockTypeSet.empty concrete_blocks in
  let iter_check_variables k x = 
    if not (List.mem (fst k) base_block)
    then check_variables x final_blocks 
  in
    ConcreteBlockMap.iter iter_check_variables final_blocks ;
    circuit_start, final_blocks



    
    
