let parse_integer integer_string =
  Parser.integer_start Lexer.token (Lexing.from_string integer_string)

let lex_file filename =
  let chan = open_in filename in
  let t = Lexing.from_channel chan in
  let rec process acc =
    match Lexer.token t with
      | Parser.EOF -> List.rev acc
      | x -> process (x::acc)
  in process []

let parse_file filename = 
  let chan = open_in filename in
  let res = Parser.circuit Lexer.token (Lexing.from_channel chan) in
    res


let mk_string ?(b="") ?(e="") ?(sep="") f l =
  let open List in
  let body =  match l with
      | [] -> ""
      | [ x ] -> f x
      | x::xs -> fold_left (fun s x -> s ^ sep ^ (f x)) (f x) xs in
    b ^ body ^ e

(** fonction d'aide pour filtrer les erreurs*)
let wire_identifier_to_string = function
  | None, s2 -> s2
  | (Some s1), s2 -> s1 ^"."^s2


let dump_file filename =
  let (start,l,d) = parse_file filename in
  let open Ast.Integer in
  let open Ast.IntegerAst in
  let open Printf in
  let string_of_bop = function
      | Plus   -> "+"
      | Minus  -> "-"
      | Times  -> "*"
      | Div    -> "/"
      | Mod    -> "%"
  in
  let string_of_uop = function
    | Neg -> "-"
  in
  let rec string_of_integer = function
    | Int i -> string_of_int i
    | Var s -> s
    | Binary_Op (bop, i1, i2) -> 
	"(" ^ (string_of_integer i1) ^ " " ^ (string_of_bop bop) ^ " " ^ (string_of_integer i2) ^ ")"
    | Unary_Op (uop, i) -> (string_of_uop uop) ^ (string_of_integer i)
  in
  let print_parameter x = printf "%s, " (string_of_integer x)
  in
  let print_input (s,i) = printf "%s -> %s, " s (string_of_integer i) in
  let print_instanciation inst = printf "%s, " (fst inst.block_type) in
  let print_output (o,_) = printf "%s -> %s, " (fst o) (string_of_integer (snd o))  in
  let print_block { name ; parameters ; inputs ; instantiations ; outputs } =
    printf "%s : \n\tparameters : " name ;
    List.iter print_parameter parameters ;
    printf "\n\tinputs : ";
    List.iter print_input inputs ;
    printf "\n\tinstantiations : ";
    List.iter print_instanciation instantiations;
    printf "\n\toutputs : ";
    List.iter print_output outputs;
    printf "\n\n\n"
  in
    List.iter print_block l 

module IntAstPrinter =
struct
  open Ast.IntAst
  open Printf
 
  let print_output (o,_) = sprintf "%s -> %s" (fst o) (string_of_int (snd o))  
  let print_input (s,i) = sprintf "%s -> %s" s (string_of_int i) 

  let print_block x =
    printf "%s : \n\tparameters : %s\n\tinputs : %s\n\tinstantiations : %s\n\toutputs : %s\n\n\n" 
      x.name 
      (mk_string ~sep:", " string_of_int x.parameters)
      (mk_string ~sep:", " print_input x.inputs)
      (mk_string ~sep:", " (fun z -> fst z.block_type) x.instantiations)
      (mk_string ~sep:", " print_output x.outputs)

  let print_block_type x = 
    printf "%s < %s > :\n" (fst x) (mk_string ~sep:", " string_of_int (snd x))

  let print (_,l,_) = SemanticAnalysis.ConcreteBlockMap.iter 
    (fun k x -> print_block_type k ; print_block x) l
    
  let rec string_of_wire = function
    | Named_Wire wi -> wire_identifier_to_string wi
    | Merge l -> mk_string ~b:"{ " ~e:" }" ~sep:", " string_of_wire l
    | Slice s -> sprintf "slice %s min %d max %d" (wire_identifier_to_string s.wire) s.min s.max

end


let localize pos =
  let open Lexing in
  let open Printf in
  let car_pos = pos.pos_cnum - pos.pos_bol + 1 in
    printf "fichier %s : ligne %d, caractères %d-%d:\n" pos.pos_fname pos.pos_lnum (car_pos-1) car_pos

open Printf
open Pattern
open SemanticAnalysis
open IntegerToInt

let string_of_block_type bt =
  sprintf "%s < %s >" (fst bt) (mk_string ~sep:", " string_of_int (snd bt))

let rec analyse_exception = function
  | Free_Variable s -> printf "variable libre : %s\n" s
  | Zero_Sized_Wire s -> printf "fil de taille nulle %s\n" s
  | Bad_Pattern_Parameter -> printf "pattern incorrect\n"
  | Failed_unification -> printf "unification ratée\n"
  | Same_name s -> printf "deux variables du même nom : %s\n" s
  | Instance_not_found bt -> printf "block non défini : %s\n" (string_of_block_type bt)
  | Bad_recursion (btl,s) -> printf "erreur de récursion : %s\nbacktrace : %s\n" s 
      (mk_string ~sep:" -> " string_of_block_type btl)
  | Bad_pattern_number bt -> printf "pas le bon nombre de patterns : %s\n" (string_of_block_type bt)
  | Variable_not_found wi -> printf "variable non définie : %s\n" (wire_identifier_to_string wi)
  | Loop wi -> printf "présence d'une boucle : %s\n" (wire_identifier_to_string wi)
  | Slice_incorrect wi -> printf "slice incorrecte : %s\n" (wire_identifier_to_string wi)
  | Number_of_arguments inst -> printf "nombre d'arguments : %s\n" inst.Ast.IntAst.var_name
  | Bad_sized_wire w -> printf "fil de mauvaise taille : %s" (IntAstPrinter.string_of_wire w)
  | Bad_block_definition (s,il,ex) -> printf "problème lors de la définition du block %s %s\n" s
      (mk_string ~b:"< " ~e:" >" ~sep:", " string_of_int il) ; analyse_exception ex
  | e -> raise e


let main filename =
  let open Printf in
  let chan = open_in filename in
  let buf = Lexing.from_channel chan in
    try
      let res = Parser.circuit Lexer.token buf in
	SemanticAnalysis.analyse_circuit res
    with 
	Lexer.Lexing_error c ->
	  localize (Lexing.lexeme_start_p buf);
	  printf "Erreur dans l'analyse lexicale: %s." c;
	  exit 1
      | Parser.Error ->
	  localize (Lexing.lexeme_start_p buf);
	  printf "Erreur dans l'analyse syntaxique.";
	  exit 1
      | e -> analyse_exception e ; failwith ""
