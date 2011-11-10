open Ast

module type IntModulePrinterType =
sig
  include AstModuleType
  val integer : integer -> string
end

module IntegerPrinter =
struct

  open Integer
  include IntegerAst
  
  let string_of_bop = function
    | Plus   -> "+"
    | Minus  -> "-"
    | Times  -> "*"
    | Div    -> "/"
    | Mod    -> "%"
    | Power  -> "^" 
	
  let string_of_uop = function 
    | Neg -> "-"
  
  let rec integer = function
    | Int i -> string_of_int i
    | Var s -> s
    | Binary_Op (bop, i1, i2) -> 
	"(" ^ (integer i1) ^ " " ^ (string_of_bop bop) ^ " " ^ (integer i2) ^ ")"
    | Unary_Op (uop, i) -> (string_of_uop uop) ^ (integer i)
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

let print_integer_ast (start,list,devices) =
  let open IntegerAstPrinter in
  let open Printf in
    printf "start %s\n\n\n" (block_type start) ; 
    printf "%s" (String.concat "\n" (List.map block_type_definition list)) ;
    printf "%s" (mk_string ~b:"device list :\n" ~sep:"\n" 
		    (fun (id,n) -> sprintf "> %s -> %s" id (string_of_int n)) devices)

let print_int_ast (start,map,devices) = 
  let open IntAstPrinter in
  let open Printf in
    printf "start %s\n\n\n" (block_type start) ; 
    BaseBlocks.ConcreteBlockMap.iter 
      (fun k x -> printf "%s :\n%s" (block_type k) (block_type_definition x)) map ;
    printf "%s" (mk_string ~b:"device list :\n" ~sep:"\n" 
		    (fun (id,n) -> sprintf "> %s -> %s" id (string_of_int n)) devices)
