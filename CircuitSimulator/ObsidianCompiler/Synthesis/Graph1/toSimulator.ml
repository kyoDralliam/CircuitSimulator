open Typesgraphe

let main (graphe,_,_,_) =

  let string_of_gate = function
    | VideIntersideral -> failwith "VideIntersideral"
    | Bit Zero -> failwith "0"
    | Bit Un -> failwith "1"
    | Non -> "N"
    | Ou -> "O"
    | Et -> "A"
    | Xor -> "X"
    | Entree -> "E"
    | Multiplexer -> failwith "M"
    | Registre -> "R"
    | Sortie -> "S"
    | Device _ -> failwith "device non supporté"
  in

  let string_of_node (gate, out_list) =
    (string_of_gate gate) ^ " " ^ 
      (string_of_int (List.length out_list)) ^ " " ^ 
      (String.concat " " (List.map 
        (fun (x,y) -> string_of_int x) 
        out_list))
  in 

  let string_list = List.map string_of_node (Array.to_list graphe) in

  (string_of_int (Array.length graphe)) ^ "\n" ^
    String.concat "\n" string_list
