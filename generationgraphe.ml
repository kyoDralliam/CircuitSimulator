open Typesgraphe
open BaseBlocks

(* Fonction qui concatène deux circuits sans rien vérifier ni rien modifier *)
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
    let g = ref [||] in
    let ajouter t = 
        g := Array.append g t;
    in

    (* Si il existe une instantiation du bloc que l'on ne connaît pas, on crée
     *   d'abord le circuit associé pour le rajouter dans la map et pouvoir s'en
     *   servir. *)
    iter (fun x -> if not (CBM.mem (x.block_type) table) then
                                creercircuit env (CBM.find x.block_type env))
         bloc.instantiations;

    (* Maintenant, toutes les instantiations utilisées sont connues : on les
     *   concatène comme un gros sac. *)
    (* On commence par ajouter les entrées *)
    iter (fun (_,n) -> ajouter (Array.make n (Entree,[]),
                                creerliste n,
                                [],
                                [])) 
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



let _ =
    List.iter (fun x -> CBM.add (x.name,x.parameters) (circuitbasique x))
              base_block