open AstToGraph

(** FORMAT DE SORTIE DU GRAPHE :
    1ere ligne -> nombre de portes
    puis sur chaque lignes ->
        nom de la porte ("0", "1", "M", "N", "O", "A", "X", "E", "R", "S")
        suivit du nombre de portes en sortie
        puis des numéros de chaque porte 
*)


let rec fold_left_for f x debut fin =
  if debut <= fin then
    fold_left_for f (f x debut) (debut + 1) fin
  else
    x

type tri_topologique_etat =
  | Non_Traite
  | En_Traitement
  | Traite

let tri_topologique graphe =
  
  let nouvelle_num = Array.make (Array.length graphe) 0 in
  let ancienne_num = Array.make (Array.length graphe) 0 in
  let parcouru = Array.make (Array.length graphe) Non_Traite in
  
  let rec parcourir prochain_num_attribue noeud_ancienne =
    match parcouru.(noeud_ancienne) with
      | Non_Traite ->
	  (
	    parcouru.(noeud_ancienne) <- En_Traitement;

	    let prochain_num_attribue =
	      match fst graphe.(noeud_ancienne) with
		| Register | Device _ -> prochain_num_attribue
		| _ -> 
		    List.fold_left
		      (fun prochain_num_attribue (fils_ancienne,_) ->
			 parcourir prochain_num_attribue fils_ancienne)
		      prochain_num_attribue
		      (try (snd graphe.(noeud_ancienne)).(0) with _ -> []) 	      
	    in
		
	    nouvelle_num.(noeud_ancienne) <- prochain_num_attribue;
	    ancienne_num.(prochain_num_attribue) <- noeud_ancienne;

	    parcouru.(noeud_ancienne) <- Traite;

	    prochain_num_attribue - 1
	  )
      | En_Traitement ->
          failwith "Cycle !"
      | Traite -> prochain_num_attribue
	  
  in  
  
  assert
    (fold_left_for
      (fun prochain_num_attribue noeud_ancienne ->
        parcourir prochain_num_attribue noeud_ancienne)
      (Array.length graphe - 1)
      0
      (Array.length graphe - 1) = -1);

  (nouvelle_num, ancienne_num)

let string_of_graphe graphe =

  let string_of_gate = function
    | Gnd -> "0"
    | Vdd -> "1"
    | Not -> "N"
    | Or -> "O"
    | And -> "A"
    | Xor -> "X"
    | Input n -> if n > 1 then failwith "entrée à un seul bit supportées" else "E"
    | Multiplexer -> "M"
    | Register -> "R"
    | Output n -> if n > 1 then failwith "sortie à un seul bit supportées" else "S"
    | Device _ -> failwith "device non supporté"
  in

  let (nouvelle_num, ancienne_num) = tri_topologique graphe in

  let string_of_noeud (porte, liste_sorties_tableau) =
    let liste_sorties = try liste_sorties_tableau.(0) with _ -> [] in
    (string_of_gate porte) ^ " " ^ 
      (string_of_int (List.length liste_sorties)) ^ " " ^ 
      (String.concat " " (List.map 
        (fun (x,y) -> (string_of_int nouvelle_num.(x)) ^ " " ^ (string_of_int y)) 
	liste_sorties ))
  in

  let resultat = ref ((string_of_int (Array.length graphe)) ^ "\n") in

  for i = 0 to Array.length graphe - 1 do
    resultat := !resultat ^ (string_of_noeud graphe.(ancienne_num.(i))) ^ "\n"
  done;
    
  !resultat
      
