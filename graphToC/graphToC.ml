(* #use "typesgraphe.ml";; *)
(* #load "str.cma";; *)
open Typesgraphe

let gates_outputs_array_name = "gates_outputs"
let registers_array_name = "registers"
let circuit_inputs_array_name = "circuit_inputs"
let circuit_outputs_array_name = "circuit_outputs"

let first_input_number = 1

let file_slurp name =
  let input_channel = open_in name in
  let file_size = in_channel_length input_channel in
  let file_content = String.create file_size in
  really_input input_channel file_content 0 file_size;
  close_in input_channel;
  file_content

let text_before = file_slurp "text_before"
let text_after = file_slurp "text_after"

let rec fold_left_for f x first last =
  if first <= last then
    fold_left_for f (f x first) (first + 1) last
  else
    x

let rec replace_vars s = function
  | [] -> s
  | (k,v)::t -> let r = Str.regexp ("\\$(" ^ k ^ ")") in
    replace_vars (Str.global_substitute r (fun _ -> v) s) t

type topological_sort_state =
  | Not_Processed
  | Being_Processed
  | Processed

let topological_sort graph circuit_inputs circuit_outputs =
 
  let used_space = Array.fold_left 
    (fun n (gate,_) -> if gate = VideIntersideral then n else n+1)
    0
    graph
  in

  let new_num = Array.make (Array.length graph) (-1) in
  let old_num = Array.make used_space (-1) in
  let node_state = Array.make (Array.length graph) Not_Processed in  

  let rec explore next_num_to_give node_old =
    match node_state.(node_old) with
      | Not_Processed ->
	(
	  node_state.(node_old) <- Being_Processed;
	  
	  let next_num_to_give =
	    match fst graph.(node_old) with
	      | Registre | Device _ -> next_num_to_give
	      | _ ->
		  List.fold_left
		    (fun next_num_to_give l ->
                      List.fold_left
		        (fun next_num_to_give (fils_old,_) ->
		          explore next_num_to_give fils_old)
		        next_num_to_give
                        l)
                    next_num_to_give
        	    (snd graph.(node_old))
	  in
	  
	  new_num.(node_old) <- next_num_to_give;
	  old_num.(next_num_to_give) <- node_old;
	  
	  node_state.(node_old) <- Processed;
	  
	  next_num_to_give - 1
	)
      | Being_Processed ->
        failwith "Cycle !"
      | Processed -> next_num_to_give
	
  in

  let explore_list next_num_to_give l =
    List.fold_left
      (fun next_num_to_give node_old ->
        if fst graph.(node_old) <> VideIntersideral then
	  explore next_num_to_give node_old
        else
	  next_num_to_give)
      next_num_to_give
      l
  in

  let next_num_to_give = explore_list (used_space - 1) 
    (List.rev circuit_outputs) in
  let next_num_to_give = explore_list next_num_to_give
    (List.rev circuit_inputs) in

  assert
    (fold_left_for
       (fun next_num_to_give node_old ->
         if fst graph.(node_old) <> VideIntersideral then
	   explore next_num_to_give node_old
	 else
	   next_num_to_give)
       next_num_to_give
       0
       (Array.length graph - 1) = -1);

  (new_num,old_num)


let gates_outputs_positions graph old_num =
  
  (* Les 2 premières sorties sont Gnd et Vdd *)
  let number_of_outputs = ref 2 in

  let positions = Array.make (Array.length old_num) (0,0) in
  
  for i = 0 to Array.length old_num - 1 do
    (* On ne crée pas une sortie pour chaque constante :
       la sortie 0 sert pour tous les Gnd, et la sortie 1 pour tous les Vdd *)
    let (gate, outputs) = graph.(old_num.(i)) in
    match gate with
      | Bit Zero -> 
	positions.(i) <- (0,1);
      | Bit Un ->
	positions.(i) <- (1,1);
      | _ -> 
	let l = List.length outputs in
	positions.(i) <- (!number_of_outputs, l);
	number_of_outputs := !number_of_outputs + l;        
  done;
  
  (positions, !number_of_outputs)

let expected_number_of_inputs = function
  | Bit _ -> 0
  | Non | Ou | Et | Xor -> 2
  | Entree -> 0
  | Multiplexer -> 3
  | Registre -> 1
  | Sortie -> 1
  | Device _ -> 67
  | VideIntersideral -> 0
    
let gates_inputs_positions graph new_num old_num outputs_positions =
  
  let number_of_inputs = ref 0 in
  let number_of_connected_inputs = ref 0 in
  
  let inputs_positions = Array.init
    (Array.length old_num)
    (fun i ->
      let l = (expected_number_of_inputs (fst graph.(old_num.(i)))) in
      number_of_inputs := !number_of_inputs + l;
      Array.make l (-1))
  in
  
  for current_node = 0 to Array.length old_num - 1 do
    ignore
      (List.fold_left
	(fun output_number output ->
	  List.iter
	     (fun (target_node, target_input) ->
	       (* On vérifie que l'entrée n'était pas déjà connectée *)
	       assert
                 (inputs_positions
                   .(new_num.(target_node))
                   .(target_input - first_input_number)
                 = -1);
	       inputs_positions
                 .(new_num.(target_node))
                 .(target_input - first_input_number)
	       <- fst outputs_positions.(current_node) + output_number;
	       incr number_of_connected_inputs)
	    output;
	  output_number + 1)
	0
	(snd graph.(old_num.(current_node))))
  done;
  
  (* On vérifie que toutes les entrées sont connectées *)
  assert (!number_of_inputs = !number_of_connected_inputs);
  
  (inputs_positions, !number_of_inputs)

let registers_outputs_positions graph old_num registers outputs_positions =
  
  let registers_outputs_positions = Array.make (List.length registers) (-1) in

  let current_register = ref 0 in
  
  for current_node = 0 to Array.length old_num - 1 do
    match fst graph.(old_num.(current_node)) with
      | Registre ->
	(* On vérifie qu'il n'y a pas plus de registres que prévu *)
	assert (!current_register < Array.length registers_outputs_positions);
	registers_outputs_positions.(!current_register)
	 <- fst outputs_positions.(current_node);
	incr current_register
      | _ -> ()
  done;
  
  (* On vérifie qu'il n'y a pas moins de registres que prévu *)
  assert (!current_register = Array.length registers_outputs_positions);
  
  registers_outputs_positions

let circuit_inputs_positions graph old_num circuit_inputs
    gates_outputs_positions =
  
  let circuit_inputs_positions = Array.make (List.length circuit_inputs) (-1) in
  
  let current_circuit_input = ref 0 in

  for current_node = 0 to Array.length old_num - 1 do
    match fst graph.(old_num.(current_node)) with
      | Entree ->
	(* On vérifie qu'il n'y a pas plus d'entrées que prévu *)
	assert (!current_circuit_input < Array.length circuit_inputs_positions);
	circuit_inputs_positions.(!current_circuit_input)
         <- fst gates_outputs_positions.(current_node);
        incr current_circuit_input
      | _ -> ()
  done;

  (* On vérifie qu'il n'y a pas moins d'entrées que prévu *)
  assert (!current_circuit_input = Array.length circuit_inputs_positions);

  circuit_inputs_positions

let circuit_outputs_positions graph old_num circuit_outputs 
    gates_inputs_positions =

  let circuit_outputs_positions = 
    Array.make (List.length circuit_outputs) (-1)
  in
  
  let current_circuit_output = ref 0 in
  
  for current_node = 0 to Array.length old_num - 1 do
    match fst graph.(old_num.(current_node)) with
      | Sortie ->
	(* On vérifie qu'il n'y a pas plus de sorties que prévu *)
	assert
	  (!current_circuit_output < Array.length circuit_outputs_positions);
	circuit_outputs_positions.(!current_circuit_output)
         <- gates_inputs_positions.(current_node).(0);
	incr current_circuit_output
      | _ -> ()
  done;

  (* On vérifie qu'il n'y a pas moins de sorties que prévu *)
  assert (!current_circuit_output = Array.length circuit_outputs_positions);
 
  circuit_outputs_positions

let gate_operation_code gate inputs_codes =
  (* On vérifie que l'on reçoit le bon nombre d'arguments *)
  assert (Array.length inputs_codes = expected_number_of_inputs gate);
  match gate with
    | Bit Zero -> "0"
    | Bit Un -> "1"
    | Non -> "!" ^ inputs_codes.(0)
    | Et -> inputs_codes.(0) ^ " && " ^ inputs_codes.(1)
    | Ou -> inputs_codes.(0) ^ " || " ^ inputs_codes.(1)
    | Xor -> inputs_codes.(0) ^ " != " ^ inputs_codes.(1)
    | Entree -> ""
    | Multiplexer ->
      inputs_codes.(0) ^ " ? " ^ inputs_codes.(2) ^ " : " ^ inputs_codes.(1)
    | Registre -> inputs_codes.(0)
    | Sortie -> inputs_codes.(0)
    | Device _ -> failwith "Not implemented 1 in GraphToC.gate_operation_code"
      (* FIXME *)
    | VideIntersideral -> failwith "TSNH 1 in GraphToC.gate_operation_code"

let node_code graph old_num gates_inputs_positions gates_outputs_positions
    next_register node_new =
  
  let inputs_codes = Array.map
    (fun input_position ->
      gates_outputs_array_name ^ "[" ^ (string_of_int input_position) ^ "]")
    gates_inputs_positions.(node_new)
  in

  let gate = fst graph.(old_num.(node_new)) in
  
  let gate_code = 
    gate_operation_code gate inputs_codes
  in
  
  let code =
    match gate with
      | Bit _ -> ""
        (* Les constantes sont initialisées au début de la simulation *)
      | Entree -> ""
        (* Les entrées aussi *)
      | Sortie -> ""
        (* Les sorties sont traitées à la fin de la simulation *)
      | Registre ->
	  registers_array_name ^ "[" ^
            (string_of_int next_register) ^ "] = " ^
	    gate_code
      | Device _ -> failwith "Not implemented 1 in GraphToC.node_code"
        (* FIXME *)
      | _ ->
	  gates_outputs_array_name ^ "[" ^
            (string_of_int (fst gates_outputs_positions.(node_new))) ^ "] = " ^
	    gate_code	
  in
  
  (code, match gate with Registre -> next_register + 1 | _ -> next_register)

let circuit_code (graph, circuit_inputs, circuit_outputs, registers) =
  
  let (new_num,old_num) = topological_sort graph circuit_inputs circuit_outputs in

  let (gates_outputs_positions, _) =
    gates_outputs_positions graph old_num
  in
  
  let (gates_inputs_positions, _) =
    gates_inputs_positions graph new_num old_num gates_outputs_positions
  in
  
  let registers_outputs_positions =
    registers_outputs_positions graph old_num registers gates_outputs_positions
  in
  
  let circuit_inputs_positions =
    circuit_inputs_positions graph old_num circuit_inputs
      gates_outputs_positions
  in
  
  let circuit_outputs_positions =
    circuit_outputs_positions graph old_num circuit_outputs
      gates_inputs_positions
  in
  
  let vars_map =
    [("gates_outputs_array_name", gates_outputs_array_name);
    ("registers_array_name", registers_array_name);
    ("circuit_outputs_array_name", circuit_outputs_array_name);
    ("circuit_inputs_array_name", circuit_inputs_array_name);
    ("gates_outputs_array_length",
    string_of_int(Array.length gates_outputs_positions));
    ("registers_array_length",
    string_of_int(Array.length registers_outputs_positions));
    ("circuit_outputs_array_length",
    string_of_int(Array.length circuit_outputs_positions));
    ("circuit_inputs_array_length",
    string_of_int(Array.length circuit_inputs_positions));]
  in

  let res = ref (replace_vars text_before vars_map) in

  let margin = String.make 4 ' ' in

  for i = 0 to Array.length circuit_inputs_positions - 1 do
    res := !res ^
      margin ^ gates_outputs_array_name ^
      "[" ^ (string_of_int circuit_inputs_positions.(i)) ^ "] = " ^
      circuit_inputs_array_name ^ "[" ^(string_of_int i) ^ "];\n"
  done;
  
  res := !res ^ 
    "\n" ^ 
    margin ^ "for (i = 0 ; i <= cycles ; i++ )\n" ^
    margin ^ " {\n\n";

  let margin = String.make 8 ' ' in

  for i = 0 to Array.length registers_outputs_positions - 1 do
    res := !res ^
      margin ^ gates_outputs_array_name ^
      "[" ^ (string_of_int registers_outputs_positions.(i)) ^ "] = " ^
      registers_array_name ^ "[" ^(string_of_int i) ^ "];\n"
  done;
  
  res := !res ^ "\n";

  ignore
    (fold_left_for
      (fun next_register i -> 
        let (code, next_register) = 
          node_code graph old_num gates_inputs_positions
            gates_outputs_positions next_register i
        in
        if code <> "" then res := !res ^ margin ^ code ^ ";\n";
        next_register)
      0
      0
      (Array.length old_num - 1));

  res := !res ^ 
    "\n" ^ 
    margin ^ "if (i == cycles || step_by_step )\n" ^
    margin ^ " {\n";
  
  let margin = String.make 12 ' ' in
  
  for i = 0 to Array.length circuit_outputs_positions - 1 do
    res := !res ^
      margin ^ circuit_outputs_array_name ^
      "[" ^  (string_of_int i) ^ "] = " ^
      gates_outputs_array_name ^ "[" ^
      (string_of_int circuit_outputs_positions.(i))  ^ "] ? '1' : '0' ;\n"
  done;

  res := !res ^ "\n" ^ margin ^
    "fwrite (" ^ circuit_outputs_array_name ^ ", 1, " ^ 
    (string_of_int (Array.length circuit_outputs_positions)) ^
    ", stdout);\n\n" ^
    margin ^ "fprintf (stdout, \"\\n\");\n";
  
  let margin = String.make 8 ' ' in
  
  res := !res ^ margin ^ " }\n\n";

  let margin = String.make 4 ' ' in
  
  res := !res ^ margin ^ " }\n\n" ^
    (replace_vars text_after vars_map);

  !res

;;


let graph = [| (Sortie, []); (Sortie, []); (Sortie, []);
            (Xor, [[(1,1);(5,1)]]); (Et, [[(0,1)]]); (Registre, [[(2,1)]]);
            (Entree, [[(3,1);(4,1)]]); (Entree, [[(3,2);(4,2)]]) |] in
let inputs = [6;7] in
let outputs = [0;1;2] in
let registers = [5] in
print_string (circuit_code (graph, inputs, outputs, registers));;
