open Typesgraphe

let rec fold_left_for f x debut fin =
  if debut <= fin then
    fold_left_for f (f x debut) (debut + 1) fin
  else
    x

let tri_topologique graphe =
  
  let nouvelle_num = Array.make (Array.length graphe) 0 in
  let ancienne_num = Array.make (Array.length graphe) 0 in
  let parcouru = Array.make (Array.length graphe) None in
  
  let rec parcourir num_parcours prochain_num_attribue noeud_ancienne =
    let prochain_num_attribue = 
      match fst graphe.(noeud_ancienne) with
        | Registre | Device _ -> prochain_num_attribue
        | _ ->
            List.fold_left
              (fun prochain_num_attribue (fils_ancienne,_) ->
                match parcouru.(fils_ancienne) with
                  | None ->
                      parcouru.(fils_ancienne) <- Some num_parcours;
                      parcourir num_parcours prochain_num_attribue fils_ancienne
                  | Some n when n = num_parcours ->
                      failwith "Cycle !"
                  | _ -> prochain_num_attribue)
              prochain_num_attribue
              (snd graphe.(noeud_ancienne))
    in
    
    nouvelle_num.(noeud_ancienne) <- prochain_num_attribue;
    ancienne_num.(prochain_num_attribue) <- noeud_ancienne;
    
    prochain_num_attribue + 1
  in  
  
  assert
    (fold_left_for
      (fun prochain_num_attribue noeud_ancienne ->
        parcourir noeud_ancienne prochain_num_attribue noeud_ancienne)
      (Array.length graphe - 1)
      0
      (Array.length graphe - 1) = -1);

  (nouvelle_num, ancienne_num)

let string_of_graphe (graphe,_,_,_) =

  let string_of_gate = function
    | Bit Zero -> "0"
    | Bit Un -> "1"
    | Non -> "N"
    | Ou -> "O"
    | Et -> "A"
    | Xor -> "X"
    | Entree -> "E"
    | Multiplexer -> "M"
    | Registre -> "R"
    | Sortie -> "S"
    | Device _ -> failwith "D"
  in

  let (nouvelle_num, ancienne_num) = tri_topologique graphe in

  let string_of_noeud (porte, liste_sorties) =
    (string_of_gate porte) ^ " " ^ 
      (string_of_int (List.length liste_sorties)) ^ " " ^ 
      (String.concat " " (List.map 
        (fun (x,y) -> (string_of_int nouvelle_num.(x)) ^ " " ^ (string_of_int nouvelle_num.(y))) 
	liste_sorties ))
  in

  let resultat = ref ((string_of_int (Array.length graphe)) ^ "\n") in

  for i = 0 to Array.length graphe - 1 do
    resultat := !resultat ^ (string_of_noeud graphe.(ancienne_num.(i))) ^ "\n"
  done;
    
  !resultat
      
