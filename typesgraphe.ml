open Ast

type bit = Zero | Un

type porte = 
        | Bit of bit 
        | Non | Ou | Et | Xor 
        | Entree
        | Multiplexer
        | Registre 
        | Sortie
        | Device of int 
(* Portes de base *)

type noeud = porte * (int*int) list 
(*            ^        ^   ^     ^
 *  type de porte      |   |     pour chacun des noeuds en sortie
 *                     |   |
 *    numéro du noeud -/   \- numéro de l'argument du noeud en sortie
 *      en sortie                    (si il a plusieurs entrées)         *)

type graphe = noeud array

(* module IntPourSet =
struct
        type t = int
        let compare = Pervasives.compare
end

module Iset = Set.Make(IntPourSet)

type registres = Iset.t *)
(* L'ensemble des numéros de noeuds correspondant aux registres, que l'on
 * conserve pour ne pas avoir à la recalculer *)

type registres = int list

type entrees = int list
type sorties = int list
(* Tableau des positions des portes d'entrée et de sortie *)

type circuit = graphe * entrees * sorties * registres

(* À titre d'exemple : 
 * Xor : ( [| (Entree,[(2,1)]) ; 
 *            (Entree,[(2,2)]) ; 
 *            (Xor,[(3,1)]) ; 
 *            (Sortie,[]) |] , 
 *         [ 0 ; 1 ] , 
 *         [ 3 ] , 
 *         [] )
 * Mux : ( [| (Entree,[(3,1)]) ; 
 *            (Entree,[(3,2)]) ; 
 *            (Entree,[(3,3)]) ;
 *            (Multiplexer,[(4,1)]) ; 
 *            (Sortie,[]) |] , 
 *         [ 0 ; 1 ; 2 ] , 
 *         [ 4 ] , 
 *         [] )
 * HalfAdder : ( [| (Entree,[(2,1);(3,1)]) ; 
 *                  (Entree,[(2,2);(3,2)]) ; 
 *                  (Xor,[(4,1)]) ; 
 *                  (Et,[(5,1)]) ; 
 *                  (Sortie,[]) ; 
 *                  (Sortie,[]) |] ,
 *               [ 0 ; 1  ] ,
 *               [ 4 ; 5 ] ,
 *               [] ) 
 * Additionneur série : ( [| (Entree,[(2,1),(5,1)]) ;
 *                           (Entree,[(2,2),(5,2)]) ;
 *                           (Xor,[(3,1),(4,1)]) ;
 *                           (Xor,[(8,1)]) ;
 *                           (Ou,[(6,1)]) ;
 *                           (Ou,[(6,2)]) ;
 *                           (Et,[(7,1)]) ;
 *                           (Reg,[(3,2),(4,2)]) ;
 *                           (Sortie,[]) |] ,
 *                        [ 0 ; 1 ] ,
 *                        [ 8 ] ,
 *                        [ 7 ] ) *)


