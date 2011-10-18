module type IntModuleType =
sig
  type integer
end


type id = string

module Integer =
struct

  (* Les opérateurs utilisés dans les expressions arithmétiques. *)
  type binary_op =
    | Plus | Minus | Times | Div | Mod 

  type unary_op =
    | Neg

  (* Une expression arithmétique construite à partir de nombres et de variables *)
  type integer =
    | Int of int
    | Var of id
    | Binary_Op of binary_op * integer * integer
    | Unary_Op of unary_op * integer

  let get_binary_op = function
    | Plus -> (+)
    | Minus -> (-) 
    | Times -> ( * ) 
    | Div -> (/)
    | Mod -> (mod)

end

module Int =
struct

  type integer = int
  
end


module Make(IntModule : IntModuleType) =
struct

  include IntModule

  (* Un fil nommé est repéré par le nom du bloc auquel il appartient et par son
     propre nom. 
     Attention conflit ! On a le choix entre :
     - garder block de type id (donc string) et de considérer que
     pour les entrées et les sorties du bloc courant, block =  ""
     - changer block en type id option ce qui sémantiquement me semble 
     plus correct, et mettre block = None pour les entrées et sorties
     du bloc courant.
  *)
  type wire_identifier = id option * id
    
  (* Une partie (contiguë) d'un fil *)
  type slice =
      {
	wire : wire_identifier ;
	min : integer ;
	max : integer 
      }
	
  (* Un fil :
     Named_Wire : Un fil nommé (entrée ou sortie du bloc courant, ou
     sortie d'un sous bloc)
     Merge : Réunion de plusieurs fils en un seul gros fil
     Slice : Une partie (contiguë) d'un fil *)
 type wire =
    | Named_Wire of wire_identifier
    | Merge of wire list
    | Slice of slice

  (* Déclaration d'un fil : on définit son nom et sa taille. *)
  type wire_declaration = id * integer
      
  (* Définition d'un fil : on le déclare, et on indique
     à quel fil il est connecté. *)
  type wire_definition = wire_declaration * wire

  (* Le type d'un bloc (par exemple Adder<42> pour un additionneur 42 bits). *)
  type block_type = id * integer list


  (* Instanciation d'un type de bloc, c'est à dire définition d'un bloc.
     On donne le type du bloc, son nom et ses entrées. *)
  type instantiation = 
      {
	block_type : block_type ;
	var_name : id ;
	input : wire list 
      }

  (* Définition d'un type de bloc *)
  type block_type_definition =
      {
	name : id ;
	parameters : integer list ;
	inputs : wire_declaration list ;
	instantiations : instantiation list ;
	outputs : wire_definition list
      }
	
  (* 
     Un circuit combinatoire : 
     - un identifiant correspondant au bloc global
     - une liste des blocs définis dans le code
  *)
  type circuit = block_type * block_type_definition list
      (* block_type * block_type_definition ConcreteBlockMap.t *)

  (* Définition d'un type de périphérique *)
  type device_type_definition = id

end
