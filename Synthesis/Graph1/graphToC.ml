(* #use "typesgraphe.ml";; *)
(* #load "str.cma";; *)
open Typesgraphe

(* On choisit les noms qu'auront les différents tableaux utilisés dans le
   code C :
*)

(* Le tableau qui contient la valeur de chaque sortie de chaque porte *)
let gates_outputs_array_name = "gates_outputs"
(* Le tableau qui contient la future valeur de chaque registre
   (à chaque cycle, après les calculs, la sortie de chaque registre est mise
   à jour en fonction de ce tableau)
*)
let registers_array_name = "registers"
(* Le tableau dans lequel on stocke les entrées du circuit, au moment où
   l'on les lit depuis l'entrée standard
*)
let circuit_inputs_array_name = "circuit_inputs"
(* Le tableau dans lequel on stocke les sorties du circuit, avant de les
   écrire sur la sortie standard *)
let circuit_outputs_array_name = "circuit_outputs"


(* Le numéro à partir duquel commence la numérotation
   des entrées d'une porte *)
let first_input_number = 1

let file_slurp name =
  let input_channel = open_in name in
  let file_size = in_channel_length input_channel in
  let file_content = String.create file_size in
  really_input input_channel file_content 0 file_size;
  close_in input_channel;
  file_content

(* On charge les fichiers qui contiennent la base du code C que l'on génère *)
let text_before = file_slurp "Resources/text_before"
let text_after = file_slurp "Resources/text_after"

let rec fold_left_for f x first last =
  if first <= last then
    fold_left_for f (f x first) (first + 1) last
  else
    x

(* Renvoie une version modifiée de s, dans laquelle on a remplacé toutes les
   instances de $(k) par v, pour tout (k,v) dans la liste passée en argument *)
let rec replace_vars s = function
  | [] -> s
  | (k,v)::t -> let r = Str.regexp ("\\$(" ^ k ^ ")") in
    replace_vars (Str.global_substitute r (fun _ -> v) s) t

type topological_sort_state =
  | Not_Processed
  | Being_Processed
  | Processed

(* Effectue un tri topologique sur graph :

    Une porte est placée avant les portes qui dépendent de ses sorties, sauf
    si cette porte est un registre ou un périphérique.

    Renvoie le couple (new_num,old_num) de tableaux d'entiers, où
    new_num.(i) donne la position du noeud i dans le tri topologique, et
     old_num.(j) donne le numéro du noeud en position j dans le tri topologique.

    Les arguments circuit_inputs et circuit_outputs doivent contenir
    respectivement la liste des entrées et la liste des sorties du circuit.
    On garantit alors que le tri topologique préserve l'ordre à l'intérieur
    de ces deux listes (si le noeud i est placé avant le noeud j dans une
    de ces listes, alors new_num.(i) < new_num.(j)).

    Si graph contient un cycle qui n'est pas "coupé" par un registre ou un
    périphérique, la fonction lève une exception.
*) 
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

(* Attribue à chaque porte une plage de positions dans le tableau
   des sorties des portes :
   
   Renvoie le couple (gates_outputs_positions, number_of_gates_outputs) :

   number_of_gates_outputs désigne le nombre de sorties de portes.
   
   gates_outputs_position.(i) = (first_output, length), où first_output
   indique le début de la plage attribuée à cette porte, et length la
   longueur de cette plage.
   (i est la position de la porte dans le tri topologique)

   Toutes les portes Gnd se voient attribuer la plage {0} et
   toutes les portes Vdd la plage {1}.
*)
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

(* Indique le nombre d'entrées d'une porte en fonction de sa nature *)
let expected_number_of_inputs = function
  | Bit _ -> 0
  | Non | Ou | Et | Xor -> 2
  | Entree -> 0
  | Multiplexer -> 3
  | Registre -> 1
  | Sortie -> 1
  | Device _ -> 71 (* cf ast.ml *)
  | VideIntersideral -> 0

(* Calcule pour chaque porte la position de chacune de ses entrées dans
   le tableau des sorties des portes :

   renvoie le couple (gates_inputs_positions, number_of_gates_inputs) :

   number_of_gates_inputs indique le nombre d'entrées de portes

   gates_inputs_positions.(i) est un tableau indiquant pour chaque
   entrée de la porte dans quelle case du tableau des sorties des
   portes on doit aller chercher la valeur de cette entrée.
   (i est la position de la porte dans le tri topologique;
   l'indice 0 du tableau correspond à l'entrée numérotée first_input_number)
*)
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

(* Calcule pour chaque registre la position de sa sortie dans le
   tableau des sorties des portes :
   
   Renvoie les tableau registers_outputs_positions :

   Soit le registre en position i dans la liste registers,
   registers_outputs_position.(i) indique la position de la sortie de ce
   registre dans le tableau des sorties des portes.
*)
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

(* Calcule pour chaque entrée du circuit la position de sa sortie dans
   le tableau des sorties des portes :

   Renvoie le tableau circuit_inputs_positions :

   Soit l'entrée en position i dans la liste circuit_inputs,
   circuit_inputs_positions.(i) indique la position de la sortie de cette
   entrée dans le tableau des sorties des portes.
*)
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

(* Calcule pour chaque sortie du circuit la case du tableau des sorties
   des portes où la sortie doit aller chercher sa valeur (c'est-à-dire
   la position de la sortie à laquelle son entrée est connectée)

   Renvoie le tableau circuit_outputs_positions :

   Soit la sortie en position i dans la liste circuit_outputs,
   circuit_outputs_positions.(i) indique la case du tableau des sorties
   des portes où cette sortie doit aller chercher sa valeur.
 *)
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

(* Crée pour une porte un code C qui renvoient la valeur de la sortie de la
   porte, à partir de codes C qui renvoient la valeur de sers entrées.

   Lève une exception si la porte a plusieurs sorties (c'est-à-dire s'il
   s'agit d'in périphérique)
*)
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
    | Device _ -> failwith "TSNH 1 in GraphToC.gate_operation_code"
    | VideIntersideral -> failwith "TSNH 2 in GraphToC.gate_operation_code"

(* Crée un code qui met à jour les cases du tableau des sorties des portes
   correspondant aux sorties d'une porte donnée (ou qui met à jour sa case
   dans le tableau des registres si cette porte est un registre :

   node_new indique la position de la porte dans le tri topologique.
*)   
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

(* Crée le code source d'un programme C qui simule l'exécution du cricuit
   passé en argument, en prenant ses entrées sur l'entrée standard (dans
   l'ordre de la liste circuit_inputs) et en écrivant ses sorties sur la
   sortie standard (dans l'ordre de la liste circuit_outputs)

   Le programme s'appelle de la façon suivante :
   nom_du_programme [-s] CYCLES
    -s : Afficher la valeur des sorties à chaque cycle (une ligne
         par cycle) plutôt qu'une seule fois, après le dernier cycle

    CYCLES : Durée de la simulation, en cycles
*)
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
