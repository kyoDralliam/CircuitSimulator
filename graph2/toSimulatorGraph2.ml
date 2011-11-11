open Typesgraphe

let main (graphe,_,_,_) =

  let string_of_gate = function
    | Gnd -> failwith "0"
    | Vdd -> failwith "1"
    | Not -> "N"
    | Or -> "O"
    | And -> "A"
    | Xor -> "X"
    | Input n -> if n > 1 then failwith "entrée à un seul bit supportées" else "E"
    | Multiplexer -> failwith "M"
    | Register -> "R"
    | Output n -> if n > 1 then failwith "sortie à un seul bit supportées" else "S"
    | Device _ -> failwith "device non supporté"
  in

  let string_of_node (gate, out_list_array) =
    let out_list = try out_list_array.(0) with _ -> [] in
      (string_of_gate gate) ^ " " ^ 
	(string_of_int (List.length out_list)) ^ " " ^ 
	(String.concat " " (List.map 
			      (fun (x,y) -> string_of_int x) 
			      out_list))
  in 

  let string_list = List.map string_of_node (Array.to_list graphe) in

  (string_of_int (Array.length graphe)) ^ "\n" ^
    String.concat "\n" string_list
