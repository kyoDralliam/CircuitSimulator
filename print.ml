open Ast

module type IntModulePrinterType =
sig
  include AstModuleType
  val integer : integer -> string
end

module IntegerPrinter =
struct

  include IntegerAst
  
  let string_of_bop = function
    | Integer.Plus   -> "+"
    | Integer.Minus  -> "-"
    | Integer.Times  -> "*"
    | Integer.Div    -> "/"
    | Integer.Mod    -> "%"
    | Integer.Power  -> "^" 
	
  let string_of_uop = function 
    | Integer.Neg -> "-"
  
  let rec integer = function
    | Integer.Int i -> string_of_int i
    | Integer.Var s -> s
    | Integer.Binary_Op (bop, i1, i2) -> 
	"(" ^ (integer i1) ^ " " ^ (string_of_bop bop) ^ " " ^ (integer i2) ^ ")"
    | Integer.Unary_Op (uop, i) -> (string_of_uop uop) ^ (integer i)
end

module IntPrinter =
struct
  include IntAst

  let integer = string_of_int
 end

let mk_string ?(b="") ?(e="") ?(sep="") f l =
  b ^ (String.concat sep (List.map f l)) ^ e

module AstPrinter(IntModulePrinter : IntModulePrinterType) =
struct
  open IntModulePrinter
  open Printf
 
  let wire_identifier = function
    | None, s2 -> s2
    | (Some s1), s2 -> s1 ^"."^s2

  let slice s = sprintf "%s[%s..%s]" (wire_identifier s.wire) (integer s.min) (integer s.max)

  let rec wire = function
    | Named_Wire wi -> wire_identifier wi
    | Merge wl -> mk_string ~b:"{ " ~e:" }" ~sep:", " wire wl
    | Slice s -> slice s

  let wire_declaration (id,size) = sprintf "%s(%s)" id (integer size)

  let wire_definition (wd,w) = sprintf "%s <- %s" (wire_declaration wd) (wire w)

  let block_type (id, il) = sprintf "%s < %s >" id (mk_string ~sep:", " integer il)

  let instantiation inst = sprintf "%s %s ( %s )" (block_type inst.block_type) 
    inst.var_name (mk_string ~sep:", " wire inst.input)
 
  let block_type_definition x =
    sprintf "%s : \n\tparameters : %s\n\tinputs : %s\n\tinstantiations : \n\t\t%s\n\toutputs : %s\n\n\n" 
      x.name 
      (mk_string ~sep:", " integer x.parameters)
      (mk_string ~sep:", " wire_declaration x.inputs)
      (mk_string ~sep:"\n\t\t" instantiation x.instantiations)
      (mk_string ~sep:", " wire_definition x.outputs)

end


module IntAstPrinter = AstPrinter(IntPrinter)
module IntegerAstPrinter = AstPrinter(IntegerPrinter)

let integer_ast_to_string (start,list,devices) =
  let open IntegerAstPrinter in
  let open Printf in
  let s1 = sprintf "start %s\n\n\n" (block_type start) in
  let s2 = sprintf "%s" (String.concat "\n" (List.map block_type_definition list)) in
  let device_to_string (id,n) = sprintf "> %s -> %s" id (string_of_int n) in
  let s3 = sprintf "%s" (mk_string ~b:"device list :\n" ~sep:"\n" 
			   device_to_string devices) in
      s1 ^ s2 ^ s3

let print_integer_ast ast = print_string (integer_ast_to_string ast)

let int_ast_to_string (start,map,devices) = 
  let open IntAstPrinter in
  let open Printf in
  let s1 = sprintf "start %s\n\n\n" (block_type start) in
  let collect k x acc = (sprintf "%s :\n%s" (block_type k) (block_type_definition x))::acc in
  let s2 = String.concat "" (BaseBlocks.ConcreteBlockMap.fold collect map []) in
  let device_to_string (id,n) = sprintf "> %s -> %s" id (string_of_int n) in
  let s3 = sprintf "%s" (mk_string ~b:"device list :\n" ~sep:"\n" 
			   device_to_string devices) in
    s1 ^ s2 ^ s3

let print_int_ast ast = print_string (int_ast_to_string ast)



module GraphPrinter =
struct
  open Printf
  open TestDestruction

  let gate gate_type gate_index = sprintf "%s (%i)" 
    (gate_to_base_block gate_type) gate_index

  (*
    ig : input_gate 
    igi : input_gate_index 
    og : output_gate 
    ogi : output_gate_index 
    igoi : input_gate_output_index 
    ogii : output_gate_input_index
  *)
  let transition ig igi og ogi igoi ogii labelize =
    let label = if labelize then sprintf "[label=\"%i:%i -> %i:%i\"]" igi igoi ogi ogii else "" in
    sprintf "\"%s\" -> \"%s\" %s;" (gate ig igi) (gate og ogi) label

  let graph ?(label=false) g =  
    let map_fun ig igi igoi (ogi,ogii) = let og = fst g.(ogi) in transition ig igi og ogi igoi ogii label in
    let mapi_fun ig igi igoi l = List.map (map_fun ig igi igoi) l in
    let mapi_fun2 i s = List.concat (Array.to_list (Array.mapi (mapi_fun (fst s) i) (snd s))) in
    let content = String.concat "\n  " (List.concat (Array.to_list (Array.mapi mapi_fun2 g))) in
    sprintf "digraph A {\n%s\n}" content

end
    

module LexerPrinter =
struct

  (* FIXME : à implémenter *)
  let token _ = "token"

end
