open Ast


(*

vérifier la taille des fils pour ne pas en avoir des vides

*)


(** Ast sur Integer : module de départ du circuit *)
module IntegerAst = Make(Integer)

(** Ast sur Int : module d'arrivée du circuit *)
module IntAst = Make(Int)

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

(** vérifie qu'un paramètre extrait d'une liste de paramètres est clos *)
let rec is_parameter_closed x = 
      let open IntegerAst in
	match x with
	  | [] -> true
	  | (Parameter_Value _ )::tail -> is_close_term tail
	  | (Parameter_Name _ )::tail -> false

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


module IntegerToInt =
struct

  exception Free_Variable

  open Integer

  let rec integer m = function
    | IntegerAst.Int n -> n
    | IntegerAst.Var s -> 
	try m.find s
	with Not_Found -> raise Free_Variable
    | IntegerAst.BinaryOp (op,l,r) ->
	(get_binary_op op) (integer m l) (integer m r)
    | IntegerAst.UnaryOp (Neg,n) -> -(integer m n)
    | _ -> failwith "cas impossible"

  let parameter m = function 
    | IntegerAst.Parameter_Value n -> n
    | IntegerAst.Parameter_Name s -> 
	try m.find s
	with Not_Found -> raise Free_Variable

  let parameter_list m = List.map (parameter m)

  let wire m = function
    | IntegerAst.Named_Wire s -> IntAst.Named_Wire s
    | IntegerAst.Merge l -> IntAst.Merge (List.map (wire m) l)
    | IntegerAst.Slice s -> IntAst.(slice { 
	s with 
	  min = (integer m) s.min ; 
	  max = (integer m) s.max 
      })

  let block_type m (n,l) = (n, List.map (integer m) l)
    
  let wire_declaration m (s,n) = s, integer m n

  let wire_definition m (wd,w) = wire_declaration m wd, wire m w

  let instanciation m ins = 
    { ins with 
	IntAst.block_type = block_type m ins.block_type
	IntAst.input = List.map (wire m) ins.input
    }

  let block_type_definition m block =
    let open IntAst in
    { block with
	parameters = parameter_list m block.parameters ;
	inputs = List.map (wire_declaration m) block.inputs ;
	instanciations = List.map (instanciations m) block.instanciations ;
	outputs = List.map (wire_definition m) block.outputs
    }
end

(** sépare une block_type_definition list en deux suivant que
    les block_type contiennent des paramétres libres *)
let split_if_closed_params =
  let split (with_params, without_params) block =
      if is_parameter_closed block.parameters 
      then with_params,block::without_params
      else block::with_params, without_params
  in List.fold_left split

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

(** parcourt récursivement (en descendant) les blocs 
    et réifie (applique les paramètres en tant que valeurs)
    les blocs qui n'existent pas encore
    block_type : IntAst.block_type
    abstract_blocks : IntegerAst.block_definition list
    concrete_blocks : IntAst.block_definition ConcreteBlockMap.t
*)
let rec reify_blocks block_type abstract_blocks concrete_blocks =
  if not ConcreteBlockMap.mem block_type 


(** point d'entrée de l'analyseur sémantique *)
let analyse_circuit circuit = 
  let with_params, without_params = 
    split_if_closed_params (snd circuit) in
  let block_definitions = 
    List.fold_left check_and_add ConcreteBlockMap.empty without_params in
    reify_blocks (fst circuit) with_params block_definitions



