open Ast

module StringMap = Map.Make(
  struct
    type t = string
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
    | Int n -> n
    | Var s -> 
	(try StringMap.find s m
	with Not_found -> raise (Free_Variable s))
    | Binary_Op (op,l,r) ->
	(get_binary_op op) (integer m l) (integer m r)
    | Unary_Op (Neg,n) -> -(integer m n)

  let integer_list m = List.map (integer m)

  let rec wire m = function
    | IntegerAst.Named_Wire s -> IntAst.Named_Wire s
    | IntegerAst.Merge l -> 
	if l <> []
	then IntAst.Merge (List.map (wire m) l)
	else raise (Zero_Sized_Wire "merge vide")
    | IntegerAst.Slice s ->
	let min = integer m s.IntegerAst.min in 
	let max = integer m s.IntegerAst.max  in
	  if max - min + 1 > 0
	  then
	    IntAst.(Slice { 
		      wire = s.IntegerAst.wire ;
		      min = min ; 
		      max = max 
		    })
	  else raise (Zero_Sized_Wire 
			(wire_identifier_to_string s.IntegerAst.wire))

  let block_type m (n,l) = (n, integer_list m l)
    
  let wire_declaration m (s,n) = s, integer m n

  let wire_definition m (wd,w) = wire_declaration m wd, wire m w

  let instantiation m ins = 
    let open IntAst in
      {
	block_type = block_type m ins.IntegerAst.block_type ;
	enable =
	  begin
	    match ins.IntegerAst.enable with
	      | None -> None 
	      | Some w -> Some (wire m w)
	  end;
	var_name = ins.IntegerAst.var_name ;
	input = List.map (wire m) ins.IntegerAst.input
      }

  let block_type_definition m block =
    let open IntAst in
      { name = block.IntegerAst.name ;
	parameters = integer_list m block.IntegerAst.parameters ;
	inputs = List.map (wire_declaration m) block.IntegerAst.inputs ;
	instantiations = List.map (instantiation m) block.IntegerAst.instantiations ;
	outputs = List.map (wire_definition m) block.IntegerAst.outputs
    }
end
