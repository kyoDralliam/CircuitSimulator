open BaseBlocks
open List
open Ast
open IntAst

type simple_wire = (int*int) option * (int*int) list
type complex_wire = simple_wire ref list

(** Portes de base *)
type gate = 
  | Gnd | Vdd
  | Not | Or | And | Xor 
  | Input of int
  | Multiplexer
  | Register 
  | Output of int
  | Device of (string * int list)


type node = gate * ((int*int) list) array
(**          ^        ^   ^     ^    ^ pour chaque sortie
 *           |        |   |     |
 *  type de porte     |   |     pour chacun des noeuds en sortie
 *                    |   |
 *   numéro du noeud -/   \- numéro de l'argument du noeud en sortie 
 *     en sortie                    (si il a plusieurs entrées)          *)
 
type graph = node array


(**                             device count
  *                  register count  |
  *               output count |     |                                    device list
  *        input count   |     |     |               enable list               |
  *                v     v     v     v                      v                  v             *)
type graph_info = int * int * int * int * (int * int * (int * int)) list * (string * int) list
type circuit = graph * graph_info

let base_blocks_to_gates = [ 
  "Gnd", Gnd ; "Vdd", Vdd ; 
  "Xor", Xor ; "And", And ; 
  "Or", Or ; "Mux", Multiplexer ; 
  "Reg", Register ; "Not", Not 
]

let gate_to_base_block = function
    Input _ -> "Input"
  | Output _ -> "Output"
  | Device (s,_) -> "Device " ^ s
  | g -> 
      let get r (s,g') = if g = g' then Some s else r in
      let res = fold_left get None base_blocks_to_gates in
	match res with
	  | Some s -> s
	  | None -> assert false

exception Not_a_base_block

let gate_of_block block device_list =
  try assoc (fst block) base_blocks_to_gates, Array.make 1 []
  with Not_found -> 
    try 
      ignore (assoc (fst block) device_list) ;
      Device block , Array.make device_prototype_output_number []
    with Not_found -> raise Not_a_base_block
  
let make_base_block_list block_type_definitions device_list block =
  let rec aux block =
    try 
      let res = [ gate_of_block block device_list ] in
	(*Printf.printf "a ::> %s\n" (Print.IntAstPrinter.block_type block) ;*)
	res
    with Not_a_base_block -> 
      let block_def = 
	try 
	  ConcreteBlockMap.find block block_type_definitions 
	with Not_found -> assert false
      in
      let map_fun x = aux x.block_type in
	concat (map map_fun block_def.instantiations)
  in
    aux block
	 

let rec range ?(acc=[]) ?(cmp=(>)) a b = if cmp a b then acc else range ~acc:(b::acc) a (b-1)

let rec collect ?(i=0) min max = function
  | [] -> 
      if i < max 
      then failwith "liste de mauvaise taille"
      else []
  | x::xs -> 
      if i >= min && i <= max 
      then x::(collect ~i:(i+1) min max xs)
      else 
	if i > min && i > max 
	then []
	else collect ~i:(i+1) min max xs

module WMap = Wire.WireIdentMap 

let main (start, block_type_definitions, device_list) =

  let new_device_list = map fst device_list in
  let new_base_blocks = map (fun x -> x.name, x.parameters) base_block in

  let block_def = 
    try
      ConcreteBlockMap.find start block_type_definitions 
    with Not_found -> assert false
  in

  let make_simple_wire () = None, [] in
  let rec make_complex_wire ?(acc=[]) = function
    | 0 -> acc
    | n -> make_complex_wire ~acc:((ref (make_simple_wire ()))::acc) (n-1)
  in

  let n_max = ref 0 in


  let make_global_input () =
    let plug_to_input complex_wire =
      let wire_count = ref 0 in
      let aux simple_wire_ref =
	simple_wire_ref := Some (!n_max,!wire_count) , snd !simple_wire_ref ;
	incr wire_count 
      in
	iter aux complex_wire ; 
	!wire_count
    in
    let process_complex_wire wd =
      let (s,i) = wd in
      let complex_wire = make_complex_wire i in
	let wire_count = plug_to_input complex_wire in
	let gate = (Input wire_count, Array.make wire_count []) in
	  incr n_max ;
	  gate, complex_wire
    in
    let input_lists, complex_wires = split (map process_complex_wire block_def.inputs) in
      input_lists, complex_wires
  in



  let make_global_output () =  
    let plug_to_output complex_wire =
      let wire_count = ref 0 in
      let aux simple_wire_ref =
	simple_wire_ref := fst !simple_wire_ref, (!n_max, !wire_count)::(snd !simple_wire_ref) ;
	incr wire_count
      in
	iter aux complex_wire ; !wire_count
    in
    let outputs = block_def.outputs in
    let add_to_local_map (local_map,acc) ((id,m),_) =
      let complex_wire = make_complex_wire m in
      let local_map' = WMap.add (Some "sortie", id) complex_wire local_map in
      let wire_count = plug_to_output complex_wire in
      let gate = Output wire_count, [| |] in
	incr n_max ;
	local_map' , gate::acc (*l'ajout en tête inverse l'ordre ce qui est géré aprés l'appel*)
    in
    let local_map,gate_list = fold_left add_to_local_map (WMap.empty,[]) outputs in
    let output_gate_list = rev gate_list in
      output_gate_list, local_map
  in

  let input_gate_list, input_list = make_global_input () in 

  let output_gate_list, output_map = make_global_output () in

  let base_block_list = make_base_block_list  block_type_definitions device_list start in
  let graph = Array.of_list ( input_gate_list @ output_gate_list @ base_block_list ) in



  let max_size = Array.length graph in

  let input_count = length block_def.inputs in
  let output_count = length block_def.outputs in
  let register_count = ref 0 in
  let device_count = ref 0 in
  let enable_list = ref [] in

    (* Printf.printf "-------\n" ; *) 

  (** Fabrique récursivement le graphe en 3 étapes :

      1) ajoute les fils en entrée du bloc et les fils 
      en sortie des sous-blocs à la wire_map

      2) s'appelle récursivement sur les sous-blocs si
      ceux ci ne sont pas de base, sinon "branche" les fils 
      sur les sous-blocs

      3) "branche" les fils interne sur les sorties
      les sorties sont modifiés par effet de bord 

     @param block_type
     @param liste liste de gros fil ( complex_wire )
     @param wire_map0 map contenant déja les fils des sorties
     @return une liste de liste de paire d'entiers correspondant aux indices des fils des sorties
  *)
  let rec make_graph block_type liste wire_map0 =
    
    let block_def = 
      try
	ConcreteBlockMap.find block_type block_type_definitions 
      with Not_found -> failwith ("ast_to_graph error make_graph " ^ (fst block_type)) 
    in

(* inutile à posteriori
    let wi_size_map = Wire.get_wire_identifier_map_size 
      block_def block_type_definitions device_list in
    let get_size wi = Wire.wire_size wi_size_map wi in
*)
    (* complex_wire WMap.t *)
    let wire_map = ref wire_map0 in 


    (**
       @param (s,i) wire_declaration provenant d'un input
       @param indexes liste des paire d'indices passés en argument
    *)
    let add_input (s,i) complex_wire = 
      assert (i = length complex_wire) ; 
      wire_map := WMap.add (None, s) complex_wire !wire_map 
    in

    (** prend en paramètre un tableau associatif wire_map et
	une instantiation d'un bloc et rajoute un fil pour 
	chaque sortie du bloc dans la wire_map ainsi que 
	dans une local_map qui est ensuite retourné
	@param wire_map (complex_wire WMap.t ref)
	@param inst (IntAst.instantiation)
	@return IntAst.instantiation * complex_wire WMap.t
    *)
    let add_output_instantiation inst =
      let inst_outputs = 
	try
	  let block = 
	    ConcreteBlockMap.find inst.block_type block_type_definitions in 
	    block.outputs
	with Not_found ->
	  try 
	    ignore (assoc (fst inst.block_type) device_list) ;
	    device_prototype_.outputs
	  with Not_found -> assert false
      in
      let add_to_map ((s,i),_) =
	wire_map := WMap.add (Some inst.var_name, s) (make_complex_wire i) !wire_map
      in
	iter add_to_map inst_outputs ;
	let add_to_local_map local_map ((id,_),_) =
	  let indexes =
	    try
	      WMap.find (Some inst.var_name, id) !wire_map
	    with Not_found -> assert false
	  in
	  WMap.add (Some "sortie", id) indexes local_map
	in
	let local_map = fold_left add_to_local_map WMap.empty inst_outputs in
	  inst,local_map
    in
      


      
    (** étant donné un wire, fabrique le complex_wire 
	correspondant et le retoune 
    *)
    let rec make_wire = function
      | Named_Wire wi -> 
	  (try WMap.find wi !wire_map 
	  with Not_found -> assert false)
      | Merge wl -> (concat (map make_wire wl))
      | Slice s -> 
	  let wire = 
	    try WMap.find s.wire !wire_map 
	    with Not_found -> assert false in
	  collect s.min s.max wire
    in



    (*
       _____   k0           k   _______  j           j1   _______
      |  A  | ---------------> |   B   | --------------> |   C   |
      |  i0 | --...        k'  |   i   |                 |   i1  |
      |_____| k'0       ...--> |_______|                 |_______|
      
      Le bloc B est celui en train d'être traité
      Dans le cas d'un bloc de base (non device)
      il n'a qu'une seule sortie  ( j = 0 )
      
      Partie comune avec les devices :
      
      le fil n° k en entrée du bloc B est soit de la 
      forme ((Some (i0,k0)), [| [ .. ] ; ...|])
      ^ case n° k0
      soit de la forme (None, [| [ .. ] ; ...|])
      que l'on transforme respectivement en 
      ((Some (i0,k0)), [| ... ; [ ..; (i,k) ;.. ] ; ...|])
      ou en
      (None, [| ... ; [ ..; (i,k) ;.. ] ; ...|])

    *)
      
    let call_instantiation (inst,local_map) = 
      let process_input_wire i w =
	let plug j w' =
	  begin
	    match fst !w' with
	      | Some (input_index, output_num) ->
		  assert ( input_index < max_size) ;
		  let gate,output_array = graph.(input_index) in
		    assert ( output_num < Array.length output_array ) ;
		    output_array.(output_num) <- (!n_max,j)::output_array.(output_num) 
	      | None -> ()
	  end ;
	  w' := fst !w', (!n_max,i)::(snd !w') ;
	  j+1
	in
	  fold_left plug i (make_wire w)
      in
      let process_output_wire out i output_wire =
	  output_wire := (Some (!n_max,i)), snd !output_wire ;
	    assert ( i < Array.length out) ;
	    out.(i) <- (snd !output_wire) @ out.(i) ;
	    i + 1
      in
      let process_base_block_or_device output_names assertion =
	assert( !n_max < max_size ) ;
	let _,out = graph.(!n_max) in
	let output_wires = 
	  let open List in
	  let block_name = Some inst.var_name in
	  let find_block s = 
	    try WMap.find (block_name,s) !wire_map 
	    with Not_found -> assert false in
	    concat (map find_block output_names) 
	in
	  ignore (fold_left process_input_wire 0 inst.input );
	  assertion output_wires ;
	  ignore (fold_left (process_output_wire out) 0 output_wires);
	  incr n_max
      in
      let n_enable,w_enable = 
	match inst.enable with
	  | None -> -1 , ref (None,[])
	  | Some w -> !n_max, match make_wire w with
	      | [x] -> x
	      | _ -> assert false
      in
	begin
	  if mem inst.block_type new_base_blocks 
	  then 
	    begin
	      assert (fst (gate_of_block inst.block_type device_list) = fst graph.(!n_max)) ;
	      (if fst graph.(!n_max) = Register then incr register_count);
	      process_base_block_or_device ["o"] (fun l -> assert (length l = 1))
	    end
	  else  
	    if mem (fst inst.block_type) new_device_list 
	    then 
	      begin
		assert (fst (gate_of_block inst.block_type device_list) = fst graph.(!n_max)) ;
		incr device_count ;
		process_base_block_or_device [ "data" ; "interrupt" ] 
		  (fun l -> assert (length l = device_prototype_output_number ))
	      end
	    else
	      let liste = map make_wire inst.input in
		make_graph inst.block_type liste local_map
	end ;
	if n_enable <> -1 
	then enable_list := (n_enable,!n_max-1,w_enable)::!enable_list
    in





      
    (** prend en paramètre un wire_definition et 
	le "branche" avec les fils internes au bloc
	en mettant à jour le graphe
    *)
    let plug_outputs wire_def =
      let extern_wire = 
	try WMap.find (Some "sortie", fst (fst wire_def)) !wire_map 
	with Not_found -> assert false in 
      let intern_wire = make_wire (snd wire_def) in
      let plug_simple_wire intern_wire extern_wire =
	let new_outputs = snd !extern_wire in
	let input = fst !intern_wire in
	  (match input with
	     | None -> assert false
	     | Some (input_index,output_num) -> 
		 assert ( input_index < max_size ) ;
		 let _,output_array = graph.(input_index) in
		    assert ( output_num < Array.length output_array ) ;
		   output_array.(output_num) <- new_outputs @ output_array.(output_num)) ;
	  extern_wire := input, (snd !intern_wire) @ new_outputs
      in
	assert (length extern_wire = length intern_wire) ;
	iter2 plug_simple_wire intern_wire extern_wire
    in


      iter2 add_input block_def.inputs liste ;
      let instantiations_and_local_map_list = 
	map add_output_instantiation block_def.instantiations in
	iter call_instantiation instantiations_and_local_map_list ;
	iter plug_outputs block_def.outputs

  in

    make_graph start input_list output_map ;
    let res_enable_list = List.map (fun (i,j,w) -> i, j,match fst !w with Some x -> x | None -> assert false) !enable_list in
    graph, (input_count, output_count, !register_count, !device_count, res_enable_list, device_list)
