open AstToGraph

(* On choisit les noms qu'auront les différents tableaux utilisés dans le
   code C++ :
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
   écrire sur la sortie standard
*)
let circuit_outputs_array_name = "circuit_outputs"

(* La variable représentant le périphérique numéroté i (après le tri
   topologique) a pour nom "{device_prefix}i"
*)
let device_prefix = "device_"
(* Le nom de la méthode des périquériques que l'on doit appeler
   à chaque cycle
*)
let device_method_name = "cycle"

(* Ces variables servent à stocker les versions "compressées" des entrées
   et des sorties des périphériques
*)
let (address_var_type, address_var_name) = ("unsigned int","address")
let (data_var_type, data_var_name) = ("unsigned int","data")
let (byte_enables_var_type, byte_enables_var_name) = ("char","byte_enables")

(* Le type des arguments du constructeur des périphériques *)
let constructor_argument_type = "int"
(* Les arguments du constructeur sont de la forme
   {constructor_argument_type}n
*)
let constructor_argument_prefix = "arg_"

(* Le numéro à partir duquel commence la numérotation
   des entrées d'une porte
*)
let first_input_number = 0

let file_slurp name =
  try
    let input_channel = open_in name in
    let file_size = in_channel_length input_channel in
    let file_content = String.create file_size in
    really_input input_channel file_content 0 file_size;
    close_in input_channel;
    file_content
  with
    | _ -> assert false

(* On charge les fichiers qui contiennent la base du code C++ que l'on génère *)
let text_before = file_slurp "Resources/text_before"
let text_after = file_slurp "Resources/text_after"
let device_template = file_slurp "Resources/device_template"

let rec fold_right_for f first last x =
  if first <= last then
    fold_right_for f first (last - 1) (f last x)
  else
    x

let rec fold_left_for f x first last =
  if first <= last then
    fold_left_for f (f x first) (first + 1) last
  else
    x

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

    Dans graph, les premières cases doivent correspondre aux sorties du circuit,
    les suivantes aux entrées du circuit, et les autres portes doivent être
    placées ensuite. On garantit alors que l'ordre relatif des entrées et celui
    des sorties sont préservés par le tri topologique.
    
    Si graph contient un cycle qui n'est pas "coupé" par un registre ou un
    périphérique, la fonction lève une exception.
*) 
let topological_sort graph number_of_circuit_inputs number_of_circuit_outputs =
 
  let new_num = Array.make (Array.length graph) (-1) in
  let old_num = Array.make (Array.length graph) (-1) in
  let node_state = Array.make (Array.length graph) Not_Processed in  

  let rec explore next_num_to_give node_old =
    match node_state.(node_old) with
      | Not_Processed ->
	(
	  node_state.(node_old) <- Being_Processed;
	  
	  let next_num_to_give =
	    match fst graph.(node_old) with
	      | Register | Device _ -> next_num_to_give
	      | _ ->
                  (* On traite les sorties du noeud de droite à gauche,
                     pour que les bits des entrées du circuit apparaissent
                     dans le bon ordre.
                  *)
		  Array.fold_right
		    (fun target_nodes next_num_to_give ->
                      List.fold_left
		        (fun next_num_to_give (target_node_old,_) ->
		          explore next_num_to_give target_node_old)
		        next_num_to_give
                        target_nodes)
        	    (snd graph.(node_old))
                    next_num_to_give
	  in
	  
	  new_num.(node_old) <- next_num_to_give;
	  old_num.(next_num_to_give) <- node_old;
	  
	  node_state.(node_old) <- Processed;
	  
	  next_num_to_give - 1
	)
      | Being_Processed ->
        failwith "Cycle ! (cf graphToCpp.ml)"
      | Processed -> next_num_to_give
	
  in

  (* Explore les sorties du cricuit, puis les entrées *)
  let next_num_to_give = fold_right_for
    (fun i next_num_to_give ->
      explore next_num_to_give i)
    0
    (number_of_circuit_inputs + number_of_circuit_outputs - 1)    
    ((Array.length old_num) - 1) 
  in

  assert
    (fold_left_for
      explore
      next_num_to_give
      (number_of_circuit_inputs + number_of_circuit_outputs)
      (Array.length graph - 1) = -1);
  
  (new_num,old_num)

(* Indique le nombre d'entrées d'une porte en fonction de sa nature *)
let expected_number_of_inputs = function
  | Gnd | Vdd -> 0
  | Not -> 1
  | Or | And | Xor -> 2
  | Input _ -> 0
  | Multiplexer -> 3
  | Register -> 1
  | Output n -> n
  | Device _ -> 71

(* Indique le nombre de sorties d'une porte en fonction de sa nature *)
let expected_number_of_outputs = function
  | Device _ -> 33
  | Input n -> n
  | Output _ -> 0
  | _ -> 1

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
    let l = Array.length outputs in
    (* On vérifie que la porte a le bon nombre de sorties *)
    assert (l = expected_number_of_outputs gate);
    match gate with
      | Gnd -> 
	  positions.(i) <- (0,1);
      | Vdd ->
	  positions.(i) <- (1,1);
      | _ -> 
	  positions.(i) <- (!number_of_outputs, l);
	  number_of_outputs := !number_of_outputs + l;        
  done;
  
  (positions, !number_of_outputs)

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
      (Array.fold_left
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
let registers_outputs_positions graph old_num number_of_registers outputs_positions =
  
  let registers_outputs_positions = Array.make number_of_registers (-1) in

  let current_register = ref 0 in
  
  for current_node = 0 to Array.length old_num - 1 do
    match fst graph.(old_num.(current_node)) with
      | Register ->
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

(* Calcule pour chaque entrée du circuit la position de ses sorties dans
   le tableau des sorties des portes :

   Renvoie le tableau circuit_inputs_positions :

   Soit l'entrée en position i dans la liste circuit_inputs (en partant de la
   fin), fst circuit_inputs_positions.(i) indique la position de de la sortie
   0 de cette entrée dans le tableau des sorties des portes,
   snd circuit_inputs_positions.(i) indique la taille de cette entrée.
*)
let circuit_inputs_positions graph old_num number_of_circuit_inputs
    gates_outputs_positions =
  
  let circuit_inputs_positions = Array.make number_of_circuit_inputs (0,0) in
  
  let current_circuit_input = ref 0 in

  for current_node = 0 to Array.length old_num - 1 do
    match fst graph.(old_num.(current_node)) with
      | Input n ->
	  (* On vérifie qu'il n'y a pas plus d'entrées que prévu *)
	  assert (!current_circuit_input < Array.length circuit_inputs_positions);
	  circuit_inputs_positions.(!current_circuit_input)
           <- gates_outputs_positions.(current_node);
          incr current_circuit_input
      | _ -> ()
  done;

  (* On vérifie qu'il n'y a pas moins d'entrées que prévu *)
  assert (!current_circuit_input = Array.length circuit_inputs_positions);

  circuit_inputs_positions

(* Calcule pour chaque sortie du circuit la case du tableau des sorties
   des portes où cette sortie doit aller chercher ses valeurs (c'est-à-dire
   les positions des sorties auxquelles ses entrées sont connectées).

   Renvoie le tableau circuit_outputs_positions :

   Soit la sortie en position i dans la liste circuit_outputs (en partant de la
   fin), circuit_outputs_positions.(i).(j) indique la case du tableau des
   sorties des portes où cette sortie doit aller chercher la valeur de son
   bit j.
 *)
let circuit_outputs_positions graph old_num number_of_circuit_outputs 
    gates_inputs_positions =

  let circuit_outputs_positions = 
    Array.make number_of_circuit_outputs [||]
  in
  
  let current_circuit_output = ref 0 in
  
  for current_node = 0 to Array.length old_num - 1 do
    match fst graph.(old_num.(current_node)) with
      | Output _ ->
	(* On vérifie qu'il n'y a pas plus de sorties que prévu *)
	assert
	  (!current_circuit_output < Array.length circuit_outputs_positions);
	circuit_outputs_positions.(!current_circuit_output)
         <- gates_inputs_positions.(current_node);
	incr current_circuit_output
      | _ -> ()
  done;

  (* On vérifie qu'il n'y a pas moins de sorties que prévu *)
  assert (!current_circuit_output = Array.length circuit_outputs_positions);
 
  circuit_outputs_positions

(* Crée pour une porte un code C++ qui renvoient la valeur de la sortie de la
   porte, à partir de codes C++ qui renvoient la valeur de sers entrées.

   Lève une exception si la porte a plusieurs sorties (c'est-à-dire s'il
   s'agit d'in périphérique)
*)
let gate_operation_code gate inputs_codes =
  (* On vérifie que l'on reçoit le bon nombre d'arguments *)
  assert (Array.length inputs_codes = expected_number_of_inputs gate);
  match gate with
    | Gnd -> "0"
    | Vdd -> "1"
    | Not -> "!" ^ inputs_codes.(0)
    | And -> inputs_codes.(0) ^ " && " ^ inputs_codes.(1)
    | Or -> inputs_codes.(0) ^ " || " ^ inputs_codes.(1)
    | Xor -> inputs_codes.(0) ^ " != " ^ inputs_codes.(1)
    | Input _ -> ""
    | Multiplexer ->
      inputs_codes.(0) ^ " ? " ^ inputs_codes.(2) ^ " : " ^ inputs_codes.(1)
    | Register -> inputs_codes.(0)
    | Output _ -> ""
    | Device _ -> ""
 
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
      | Gnd | Vdd -> ""
        (* Les constantes sont initialisées au début de la simulation *)
      | Input _ -> ""
        (* Les entrées aussi *)
      | Output _ -> ""
        (* Les sorties sont traitées à la fin de la simulation *)
      | Register ->
	  registers_array_name ^ "[" ^
            (string_of_int next_register) ^ "] = " ^
	    gate_code
      | Device _ -> ""
        (* failwith "Not implemented 1 in GraphToCpp.node_code" *)
        (* FIXME *)
      | _ ->
	  gates_outputs_array_name ^ "[" ^
            (string_of_int (fst gates_outputs_positions.(node_new))) ^ "] = " ^
	    gate_code	
  in
  
  (code, match gate with Register -> next_register + 1 | _ -> next_register)


(* Ajoute au buffer passé en argument le code qui définit les classes
   correspondant aux périphériques.
*)
let add_device_definitions buffer device_definitions =
  
  List.iter
    (fun (device_class_name, number_of_parameters) ->
      
      let vars_map =
        [("data_var_type", data_var_type);
        ("address_var_type", address_var_type);
        ("byte_enables_var_type", byte_enables_var_type);
        ("device_method_name", device_method_name);
        ("device_class_name", device_class_name);
        ("device_constructor_parameters",
        fold_right_for
          (fun i s -> 
            (if i = 0 then "" else ", ") ^
              constructor_argument_type ^ " " ^
              constructor_argument_prefix ^
              string_of_int i ^
              s)
          0
          (number_of_parameters - 1)
          "")]
      in
      
      (try
        Buffer.add_substitute
          buffer
          (fun k -> List.assoc k vars_map)
          device_template
      with 
        | _ -> assert false))
    
    device_definitions
    

(* Ajoute au buffer passé en argument le code qui déclare les variables
   correspondant aux périphériques et les initialise.
*)
let add_device_declarations buffer graph old_num device_definitions =

  let margin = String.make 4 ' ' in
  
  for i = 0 to Array.length old_num - 1 do
    match fst graph.(old_num.(i)) with
      | Device (name, parameters) ->
          (let number_of_parameters = List.length parameters in
          assert
            (List.exists
              ((=) (name, number_of_parameters))
              device_definitions);
          Buffer.add_string buffer
            (margin ^ name ^ " * " ^ device_prefix ^ (string_of_int i) ^
              " = new " ^ name ^ "(");
          (match parameters with
            | [] -> ()
            | h::t ->
                Buffer.add_string buffer (string_of_int h);
                List.iter
                  (fun value -> Buffer.add_string buffer
                    (", " ^ (string_of_int value)))
                  t);
          Buffer.add_string buffer "); \n")
      | _ -> ()
  done

(* Ajoute le code qui met à jour les sorties des périphériques. *)
let add_device_codes buffer graph old_num gates_inputs_positions
    gates_outputs_positions =

  let margin = String.make 8 ' ' in
  
  for i = 0 to Array.length old_num - 1 do
    match fst graph.(old_num.(i)) with
      | Device (name, parameters) ->
          
          let rec pack_inputs inputs_positions first_bit last_bit =
            if first_bit = last_bit then
              Buffer.add_string buffer
                (gates_outputs_array_name ^
                  "[" ^ (string_of_int inputs_positions.(first_bit)) ^ "]")
            else
              (Buffer.add_string buffer
                ("(" ^ gates_outputs_array_name ^
                  "[" ^ (string_of_int inputs_positions.(first_bit)) ^ "]|(");
              pack_inputs inputs_positions (first_bit+1) last_bit;
              Buffer.add_string buffer "<<1))")
          in

          Buffer.add_string buffer (margin ^ address_var_name ^ " = ");
          pack_inputs gates_inputs_positions.(i) 0 31;
          Buffer.add_string buffer "\n;";
       
          Buffer.add_string buffer (margin ^ data_var_name ^ " = ");
          pack_inputs gates_inputs_positions.(i) 32 63;
          Buffer.add_string buffer "\n;";
          
          Buffer.add_string buffer (margin ^ byte_enables_var_name ^ " = ");
          pack_inputs gates_inputs_positions.(i) 64 67;
          Buffer.add_string buffer "\n;";

          Buffer.add_string buffer
            (margin ^ data_var_name ^ " = " ^
              device_prefix ^ (string_of_int i) ^
              "->" ^ device_method_name ^ "(" ^
              address_var_name ^ ", " ^
              data_var_name ^ ", " ^
              byte_enables_var_name ^ ", \n" ^
              margin ^
              gates_outputs_array_name ^ "[" ^
              (string_of_int gates_inputs_positions.(i).(68)) ^ "], " ^
              gates_outputs_array_name ^ "[" ^
              (string_of_int gates_inputs_positions.(i).(69)) ^ "], " ^
              gates_outputs_array_name ^ "[" ^
              (string_of_int gates_inputs_positions.(i).(70)) ^ "], " ^
              gates_outputs_array_name ^ "+" ^
              (string_of_int (fst gates_outputs_positions.(i) + 32)) ^ ");\n");

          Buffer.add_string buffer
            (margin ^ gates_outputs_array_name ^ "[" ^
              (string_of_int (fst gates_outputs_positions.(i) + 32)) ^ "] = " ^
              "!!" ^ gates_outputs_array_name ^ "[" ^
              (string_of_int (fst gates_outputs_positions.(i) + 32)) ^ "];\n");
            
          for j = 0 to 31 do
            Buffer.add_string buffer
              (margin ^ gates_outputs_array_name ^ "[" ^
                (string_of_int (fst gates_outputs_positions.(i) + j)) ^ "] = (" ^
                data_var_name ^ " & 0b1" ^ (String.make j '0') ^
                ") ? 1 : 0;\n")
          done;

          Buffer.add_string buffer "\n"

      | _ -> ()
  done

(* Ajoute le code qui détruit les périphériques. *)
let add_device_deletions buffer graph old_num =
  
  let margin = String.make 4 ' ' in
  
  for i = 0 to Array.length old_num - 1 do
    match fst graph.(old_num.(i)) with
      | Device (name, parameters) ->
          Buffer.add_string buffer
            (margin ^ "delete " ^ device_prefix ^ (string_of_int i) ^ ";\n")
      | _ -> ()
  done

(* Crée le code source d'un programme C++ qui simule l'exécution du cricuit
   passé en argument, en prenant ses entrées sur l'entrée standard (dans
   l'ordre de la liste circuit_inputs) et en écrivant ses sorties sur la
   sortie standard (dans l'ordre de la liste circuit_outputs)

   Le programme s'appelle de la façon suivante :
   nom_du_programme [-s] CYCLES
    -s : Afficher la valeur des sorties à chaque cycle (une ligne
         par cycle) plutôt qu'une seule fois, après le dernier cycle

    CYCLES : Durée de la simulation, en cycles

   Les entrées du circuit doivent être placées au début du tableau
   représentant le graphe, immédiatement suivies des sorties du circuit.
   Entrées et sorties du graphes doivent de plus être placées dans leur ordre
   d'apparition dans la définition du type du bloc de base.
*)
let circuit_code (graph, (number_of_circuit_inputs, number_of_circuit_outputs,
    number_of_registers, number_of_devices, enables, device_definitions)) =
  
  ignore(enables); (* FIXME *)
  ignore(number_of_devices); (* FIXME *)
  
  let (new_num,old_num) = topological_sort graph 
    number_of_circuit_inputs number_of_circuit_outputs
  in

  let (gates_outputs_positions, gates_outputs_array_size) =
    gates_outputs_positions graph old_num
  in
  
  let (gates_inputs_positions, _) =
    gates_inputs_positions graph new_num old_num gates_outputs_positions
  in
  
  let registers_outputs_positions =
    registers_outputs_positions graph old_num 
      number_of_registers gates_outputs_positions
  in
  
  let circuit_inputs_positions =
    circuit_inputs_positions graph old_num number_of_circuit_inputs
      gates_outputs_positions
  in
  
  let circuit_outputs_positions =
    circuit_outputs_positions graph old_num number_of_circuit_outputs
      gates_inputs_positions
  in
 
  let circuit_outputs_array_size = Array.fold_left 
    (fun n o -> n + Array.length o)
    0
    circuit_outputs_positions
  in

  let circuit_inputs_array_size = Array.fold_left 
    (fun n (_,l) -> n + l)
    0
    circuit_inputs_positions
  in

  let res = Buffer.create (String.length text_before) in
  
  add_device_definitions res device_definitions;

  let vars_map =
    [("gates_outputs_array_name", gates_outputs_array_name);
    ("init_gates_outputs_array",
    if gates_outputs_array_size > 0 then " = {0}" else "");
    ("registers_array_name", registers_array_name);
    ("init_registers_array",
    if Array.length registers_outputs_positions > 0 then " = {0}" else "");
    ("circuit_outputs_array_name", circuit_outputs_array_name);
    ("circuit_inputs_array_name", circuit_inputs_array_name);
    ("gates_outputs_array_length",
    string_of_int gates_outputs_array_size);
    ("registers_array_length",
    string_of_int(Array.length registers_outputs_positions));
    ("circuit_outputs_array_length",
    string_of_int(circuit_outputs_array_size));
    ("circuit_inputs_array_length",
    string_of_int(circuit_inputs_array_size));
    ("address_var_type", address_var_type);
    ("address_var_name", address_var_name);
    ("data_var_type", data_var_type);
    ("data_var_name", data_var_name);
    ("byte_enables_var_type", byte_enables_var_type);
    ("byte_enables_var_name", byte_enables_var_name);]
  in
  
  (try
    Buffer.add_substitute res (fun k -> List.assoc k vars_map) text_before
  with 
    | _ -> assert false);

  let margin = String.make 4 ' ' in

  add_device_declarations res graph old_num device_definitions;
  Buffer.add_string res "\n";

  begin
    let position_in_circuit_inputs_array = ref 0 in
    
    for i = 0 to Array.length circuit_inputs_positions - 1 do
      for j = 0 to snd circuit_inputs_positions.(i) - 1 do
        Buffer.add_string res
          (margin ^ gates_outputs_array_name ^
          "[" ^
          (string_of_int (fst circuit_inputs_positions.(i) + j)) ^
          "] = " ^
          circuit_inputs_array_name ^ 
          "[" ^ (string_of_int (!position_in_circuit_inputs_array)) ^ "];\n");
          incr position_in_circuit_inputs_array
      done
    done
  end;
  
  Buffer.add_string res
    ("\n" ^ 
    margin ^ "gettimeofday (&simulated_time, NULL);\n" ^
    margin ^ "gettimeofday (&current_time, NULL);\n" ^
    "\n" ^
    margin ^ "for (i = 0 ; i <= cycles ; i++)\n" ^
    margin ^ " {\n" ^
    "\n" ^
    margin ^ "while (clocked && time_gt (&simulated_time, &current_time))\n" ^
    margin ^ "  gettimeofday (&current_time, NULL);\n" ^
    "\n");

  let margin = String.make 8 ' ' in

  for i = 0 to Array.length registers_outputs_positions - 1 do
    Buffer.add_string res
      (margin ^ gates_outputs_array_name ^
      "[" ^ (string_of_int registers_outputs_positions.(i)) ^ "] = " ^
      registers_array_name ^ "[" ^(string_of_int i) ^ "];\n")
  done;
  
  Buffer.add_string res "\n";

  ignore
    (fold_left_for
      (fun next_register i -> 
        let (code, next_register) = 
          node_code graph old_num gates_inputs_positions
            gates_outputs_positions next_register i
        in
        if code <> "" then  
          Buffer.add_string res (margin ^ code ^ ";\n");
        next_register)
      0
      0
      (Array.length old_num - 1));

  Buffer.add_string res "\n";
  add_device_codes res graph old_num gates_inputs_positions
    gates_outputs_positions;
  
  Buffer.add_string res
    (margin ^ "if (i == cycles || step_by_step )\n" ^
    margin ^ " {\n");
  
  let margin = String.make 12 ' ' in
  
  begin
    let position_in_circuit_outputs_array = ref 0 in

    for i = 0 to Array.length circuit_outputs_positions - 1 do
      for j = 0 to Array.length circuit_outputs_positions.(i) - 1 do
        Buffer.add_string res
          (margin ^ circuit_outputs_array_name ^
          "[" ^
          (string_of_int (!position_in_circuit_outputs_array)) ^
          "] = " ^
          gates_outputs_array_name ^
          "[" ^
          (string_of_int circuit_outputs_positions.(i).(j)) ^
          "] ? '1' : '0' ;\n");
        incr position_in_circuit_outputs_array
      done
    done
  end;

  Buffer.add_string res
    (margin ^
    "fwrite (" ^ circuit_outputs_array_name ^ ", 1, " ^ 
    (string_of_int circuit_outputs_array_size) ^
    ", stdout);\n\n" ^
    margin ^ "fprintf (stdout, \"\\n\");\n");
  
  let margin = String.make 8 ' ' in

  Buffer.add_string res (margin ^ " }\n\n");
  
  Buffer.add_string res 
    ("\n" ^
      margin ^ "add_time (&simulated_time, clock_period);\n");
  
  let margin = String.make 4 ' ' in
  
  Buffer.add_string res (margin ^ " }\n\n");

  add_device_deletions res graph old_num;
  
  Buffer.add_string res "\n";

  (try
    Buffer.add_substitute res (fun k -> List.assoc k vars_map) text_after
  with 
    | _ -> assert false);

  res

;;
