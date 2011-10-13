(* tout devrait être copié -> faire de l'ast un foncteur en gros remplacer integer par int *)

(**
   réifie les blocks :
   -> vérifie l'absence de variables libres
   -> crée les blocs dépendants d'un (ou plusieurs paramètres)
   Les paramètres :
   - ctx est une liste des paramètres (entiers)
   - block_definitions est une map block_declaration -> block_definition
*)
let rec reify ctx block_definitions =
