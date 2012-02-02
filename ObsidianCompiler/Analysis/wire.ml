open Ast
open IntAst
open List
open BaseBlocks

(** Employé pour vérifier que les variables 
    employées sont bien déclarées
*)
module WireIdentMap = Map.Make(
  struct
    type t = IntAst.wire_identifier
    let compare = compare
  end)


exception Variable_not_found of IntAst.wire_identifier

exception Loop of IntAst.wire_identifier

exception Slice_incorrect of IntAst.wire_identifier


(**
   Calcule une map qui associe à chaque wire_identifier d'un block la
   taille du fil associé.

   @param block le block à traiter (IntAst.block_type)
   @param block_definitions map contenant tous les 
   block_type_definition (IntAst.block_type_definition
   ConcreteBlockMap.t)
   @param devices liste des devices présent dans le circuit 
   ( (id * int) list )

   @return int WireIdentMap.t
*)
let get_wire_identifier_map_size block block_definitions devices =

  let add_block_variables acc inst = 
    let add var acc ((s,i),_) = WireIdentMap.add  ((Some var),s) i acc in 
      try 
	let block = ConcreteBlockMap.find 
	  inst.block_type block_definitions in
	  fold_left (add inst.var_name) acc block.outputs
      with Not_found -> 
	try 
	  ignore (assoc (fst inst.block_type) devices) ;
	  fold_left (add inst.var_name) acc device_prototype_.outputs
	with Not_found -> acc
  in

  let add_input_variables acc (x,i) =  
    WireIdentMap.add (None,x) i acc in
    
  let input_variables = fold_left add_input_variables 
    WireIdentMap.empty block.inputs in

    fold_left add_block_variables 
      input_variables block.instantiations

(** Détermine la taille d'un fil
    @param variables map qui associe la taille d'une variable (int
    WireMapIdent.t) 
    @param except paramètre optionnel permettrant de vérifier
    l'absence de boucles sur les blocks de bases qui ne sont ni des
    registres ni des devices ( id )
    @param wire le fil dont on veut déterminer la taille

    @return la taille du fil (un entier)
*)
let rec wire_size variables ?except = function
  | Named_Wire wi -> 
      if except = None || (fst wi) <> except
      then 
	try 
	  WireIdentMap.find wi variables 
	with Not_found -> raise (Variable_not_found wi)
      else raise (Loop wi)
  | Merge l -> fold_left (+) 0 (map (wire_size variables ?except) l)
  | Slice s -> 
      let size_wi = 
	try 
	  WireIdentMap.find s.wire variables 
	with Not_found -> raise (Variable_not_found s.wire) in
	if s.min >= 0 && s.max < size_wi && s.max - s.min + 1 > 0
	then s.max - s.min + 1
	else raise (Slice_incorrect s.wire)
	  
