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
  b ^ (String.concat sep (List.map f l)) ^ e



let dump_file filename = Print.print_integer_ast (parse_file filename)


let localize pos =
  let open Lexing in
  let open Printf in
  let car_pos = pos.pos_cnum - pos.pos_bol + 1 in
    printf "fichier %s : ligne %d, caractères %d-%d:\n" pos.pos_fname pos.pos_lnum (car_pos-1) car_pos

open Printf
open Pattern
open SemanticAnalysis
open IntegerToInt
open Print
open IntAstPrinter


let rec analyse_exception = function
  | Free_Variable s -> printf "variable libre : %s\n" s
  | Zero_Sized_Wire s -> printf "fil de taille nulle %s\n" s
  | Bad_Pattern_Parameter -> printf "pattern incorrect\n"
  | Failed_unification -> printf "unification ratée\n"
  | Same_name s -> printf "deux variables du même nom : %s\n" s
  | Instance_not_found bt -> printf "block non défini : %s\n" (block_type bt)
  | Bad_recursion (btl,s) -> printf "erreur de récursion : %s\nbacktrace : %s\n" s 
      (mk_string ~sep:" -> " block_type btl)
  | Bad_pattern_number bt -> printf "pas le bon nombre de patterns : %s\n" (block_type bt)
  | Variable_not_found wi -> printf "variable non définie : %s\n" (wire_identifier wi)
  | Loop wi -> printf "présence d'une boucle : %s\n" (wire_identifier wi)
  | Slice_incorrect wi -> printf "slice incorrecte : %s\n" (wire_identifier wi)
  | Number_of_arguments inst -> printf "nombre d'arguments : %s\n" inst.Ast.IntAst.var_name
  | Bad_sized_wire w -> printf "fil de mauvaise taille : %s" (wire w)
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
