open Ast

(** Ast sur Integer : module de départ du circuit *)
module IntegerAst = Make(Integer)

(** Ast sur Int : module d'arrivée du circuit *)
module IntAst = Make(Int)

module StringMap = Make(
  struct
    type t = int
    let compare = compare
  end)

(** Levée lorsqu'une variable libre est rencontrée
    lors de la transformation integer -> int
*)
exception Free_Variable of string

(** Levée si un fil se retrouve avec une taille nulle *)
exception Zero_Sized_Wire of string

(** fonction d'aide pour filtrer les erreurs*)
let wire_identifier_to_string = function
  | None, s2 -> s2
  | (Some s1), s2 -> s1 ^"."^s2

module IntegerToInt =
struct

  open Integer

  let rec integer m = function
    | IntegerAst.Int n -> n
    | IntegerAst.Var s -> 
	try StringMap.find s m
	with Not_Found -> raise (Free_Variable s)
    | IntegerAst.BinaryOp (op,l,r) ->
	(get_binary_op op) (integer m l) (integer m r)
    | IntegerAst.UnaryOp (Neg,n) -> -(integer m n)

  let integer_list m = List.map (integer m)

  let wire m = function
    | IntegerAst.Named_Wire s -> IntAst.Named_Wire s
    | IntegerAst.Merge l -> 
	if l <> []
	then IntAst.Merge (List.map (wire m) l)
	else raise (Zero_Sized_Wire "merge vide")
    | IntegerAst.Slice s ->
	let min = integer m s.min in
	let max = integer m s.max  in
	  if max - min > 0
	  then
	    IntAst.(slice { 
		      s with 
			min = min ; 
			max = max 
		    })
	  else raise (Zero_Sized_Wire 
			(wire_identifier_to_string s.wire))

  let block_type m (n,l) = (n, integer_list m l)
    
  let wire_declaration m (s,n) = s, integer m n

  let wire_definition m (wd,w) = wire_declaration m wd, wire m w

  let instanciation m ins = 
    { ins with 
	IntAst.block_type = block_type m ins.block_type
	IntAst.input = List.map (wire m) ins.input
    }

  let block_type_definition m block =
    let open IntAst in
    { block with
	parameters = integer_list m block.parameters ;
	inputs = List.map (wire_declaration m) block.inputs ;
	instanciations = List.map (instanciations m) block.instanciations ;
	outputs = List.map (wire_definition m) block.outputs
    }
end
