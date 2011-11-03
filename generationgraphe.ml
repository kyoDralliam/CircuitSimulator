open Typesgraphe
open BaseBlocks

(* Fonction qui concatène deux circuits sans rien vérifier ni rien modifier, à
 *   part les décalages d'indices *)
let concatenercircuits c1 c2 =
    match c1 with (g1,e1,s1,r1) ->
    match c2 with (g2,e2,s2,r2) ->
    let taille1 = Array.length g1 in
    let taille2 = Array.length g2 in
    let g = Array.append g1 g2 in
    for i = taille1 to taille2 + taille1 - 1 do
        match g.(i) with (porte,liste) ->
            g.(i) <- (porte,List.map (fun (a,b) -> (a+taille1,b)) liste);
    let e = List.append e1 (List.map (fun x -> x+taille1) e2) in
    let s = List.append s1 (List.map (fun x -> x+taille1) s2) in
    let r = List.append r1 (List.map (fun x -> x+taille1) r2) in
    (g,e,s,r)

(* Fonction qui génère les circuits correspondant aux blocs de base *)
let circuitbasique = function
    | xor_ -> ( [| (Entree,[(2,1)]) ; 
                   (Entree,[(2,2)]) ; 
                   (Xor,[(3,1)]) ; 
                   (Sortie,[]) |] , 
                [ 0 ; 1 ] , 
                [ 3 ] , 
                [] )
    | and_ -> ( [| (Entree,[(2,1)]) ; 
                   (Entree,[(2,2)]) ; 
                   (And,[(3,1)]) ; 
                   (Sortie,[]) |] , 
                [ 0 ; 1 ] , 
                [ 3 ] , 
                [] )
    | or_ -> ( [| (Entree,[(2,1)]) ; 
                   (Entree,[(2,2)]) ; 
                   (Or,[(3,1)]) ; 
                   (Sortie,[]) |] , 
                [ 0 ; 1 ] , 
                [ 3 ] , 
                [] )
    | mux_ -> ( [| (Entree,[(3,1)]) ; 
                   (Entree,[(3,2)]) ; 
                   (Entree,[(3,3)]) ;
                   (Multiplexer,[(4,1)]) ; 
                   (Sortie,[]) |] , 
                [ 0 ; 1 ; 2 ] , 
                [ 4 ] , 
                [] )
    | reg_ -> ( [| (Entree,[(1,1)]) ; 
                   (Registre,[(2,1)]) ; 
                   (Sortie,[]) |] , 
                [ 0 ] , 
                [ 2 ] , 
                [ 1 ] )
    | not_ -> ( [| (Entree,[(1,1)]) ; 
                   (Not,[(2,1)]) ; 
                   (Sortie,[]) |] , 
                [ 0 ] , 
                [ 2 ] , 
                [ ] )
    | _ -> failwith "Pas un bloc de base"



module CBM = SemanticsAnalysis.ConcreteBlockMap

(* La table qui contient les circuits que l'on connaît déjà *)
let table = CBM.empty

(* Le type des maps de fil *)
module MapFil = SemanticsAnalysis.WireIdentMap

(* creerliste n renvoie [0 ; 1 ; ... ; n-1] *)
let creerliste n =
    let rec parcours n = function
        | 0 -> []
        | i -> (n-i) :: parcours n (i-1)
    in 
    parcours n n

(* La fonction qui crée un circuit en partant d'une map de définitions de blocs
 *   et d'une définition de bloc, et l'ajoute dans table. *)
let rec creercircuit env bloc =
    let iter = List.iter in

    (* Le circuit que l'on renvoie et la fonction de concaténation *)
    let c = ref ([||],[],[],[]) in
    let ajouter c' = 
        c := concatenercircuits !c c';
    in

    (* Si il existe une instantiation du bloc que l'on ne connaît pas, on crée
     *   d'abord le circuit associé pour le rajouter dans la map et pouvoir s'en
     *   servir. *)
    iter (fun x -> if not (CBM.mem (x.block_type) table) then
                                creercircuit env (CBM.find x.block_type env))
         bloc.instantiations;

    (* Maintenant, toutes les instantiations utilisées sont connues : on les
     *   concatène comme un gros sac, en retenant au passage le numéro de départ
     *   de chaque bloc. *)
    (* On commence par ajouter les entrées *)
    let premiereinstance = ref 0 in
    iter (fun (_,n) -> ajouter (Array.make n (Entree,[]),
                                creerliste n,
                                [],
                                []);
                       premiereinstance := !premiereinstance + n;) 
         bloc.input;
    (* Puis, les différentes instantiations *)
    iter (fun x -> ajouter (CBM.find x.block_type env))
         bloc.instantiations;
    (* Et enfin, les sorties *)
    iter (fun ((_,n),_) -> ajouter (Array.make n (Sortie,[]),
                                    [],
                                    creerliste n,
                                    []))
         bloc.outputs;
    
    (* Pour pouvoir brancher tous les fils correctement, on doit savoir
     *   d'où ils partent. Vu que notre concaténation est grossière, on doit
     *   noter pour chaque fil le décalage correspondant. *)
    let depart = MapFil.empty in
    let compteur = ref 0 in
    (* D'abord les adresses des entrées du gros bloc *)
    iter (fun (s,n) -> MapFil.add (None,s) !compteur depart;
                       compteur := !compteur + n;)
         bloc.input;
    (* Puis, les adresses des sorties des instantiations *)
    iter (fun x -> let definition = (CBM.find x.block_type env) in
                   match (CBM.find x.block_type table) with (_,_,sorties,_) ->
                   compteur := !compteur + (List.hd sorties);
                   iter (fun ((s,n),_) -> MapFil.add (Some (fst x.block_type),s)
                                                     !compteur
                                                     depart;
                                           compteur := !compteur + n)
                        definition.outputs;)
         bloc.instantiations;

    (* La liste des positions des entrées des instantiations *)
    match !c with (_,l,_,_) ->
    let positionsentrees = List.filter (fun i -> i >= premiereinstance) l in
    
    (* On génère la table qui à un fil associe sa taille *)
    let taillefils = SemanticsAnalysis.get_wires_sizes bloc env in

    (* La liste de toutes les entrees des blocs *)
    let listeentrees = List.concat (List.map (fun x -> x.inputs)
                                             bloc.instantiations)

    (* On parcourt en parallèle la liste des entrées et la liste des positions
     *   des entrées pour faire les branchements nécessaires *)
    match !c with (g,_,_,_) ->
    let rec parcours positions entrees = match (positions,entrees) with
        | (pos,(Merge(liste))::q) -> 
            parcours pos (liste @ q)
        | (pos,(Named_Wire w)::q') when (MapFil.find w taillefils) = 0 -> 
            parcours pos q
        | (p::q,(Named_Wire w)::q') ->
            let d = MapFil.find w depart in
            (match g.(d) with (porte,liste) ->
                 g.(d) <- (porte,(p,1)::liste))
            MapFil.add w (d-1) taillefils;
            parcours q q'
        | (pos,(Slice s)::q) when s.max < s.min ->
            parcours pos q
        | (p::q,(Slice s)::q) ->
            let d = MapFil.find w depart in
            (match g.(d+min) with (porte,liste) ->
                g.(d) <- (porte,(p,1)::liste))
            parcours q (Slice {wire=s.wire ; min=s.min-1 ; max=s.max})::q
    in
    parcours positionsentrees listeentrees;

    (* On fait la même chose pour les sorties du bloc en réutilisant la fonction
     *   précédente *)
    let positionssorties = 
        List.map (fun i -> i+!compteur) (creerliste ((Array.length g)-!compteur)
    in
    let listesorties = snd (List.split bloc.outputs) in 
    parcours positionssorties listesorties;

    (* On renvoie le graphe final, non optimisé *)
    !c



    



let _ =
    List.iter (fun x -> CBM.add (x.name,x.parameters) (circuitbasique x))
              base_block
