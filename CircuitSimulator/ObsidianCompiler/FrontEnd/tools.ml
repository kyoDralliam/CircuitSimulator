let output_to_file filename s =
  let chan = open_out filename in
    output_string chan s;
    close_out chan

let output_buffer_to_file filename b =
  let chan = open_out filename in
    Buffer.output_buffer chan b;
    close_out chan

let file_slurp name =
  let input_channel = open_in name in
  let file_size = in_channel_length input_channel in
  let file_content = String.create file_size in
  really_input input_channel file_content 0 file_size;
  close_in input_channel;
  file_content

let get_files_content filenames =
  String.concat "\n" (List.map file_slurp filenames)
      

let parse_integer integer_string =
  Parser.integer_start Lexer.token (Lexing.from_string integer_string)

let parse_file filename = 
  let chan = open_in filename in
  let res = Parser.circuit Lexer.token (Lexing.from_channel chan) in
    res

let lex_file filename =
  let chan = open_in filename in
  let t = Lexing.from_channel chan in
  let rec process acc =
    match Lexer.token t with
      | Parser.EOF -> List.rev acc
      | x -> process (x::acc)
  in process []

let merge_ast (f1, (start1, blocks_def1, devices_def1)) (f2,(start2, blocks_def2, devices_def2)) =
  let start = match start1, start2 with
    | None, None -> None
    | Some x, None | None, Some x -> Some x
    | _ -> Printf.printf "Les deux fichiers %s et %s ont chacun un point d'entrée. Un seul point d'entrée est accepté.\n" f1 f2 ; exit (1)
  in
    ("mix", (start, blocks_def1@blocks_def2, devices_def1@devices_def2))

let mk_pdf pdf_name s = 
  let dot_name = Filename.temp_file pdf_name ".dot" in
    output_to_file dot_name s ;
    Sys.command ("dot -Tpdf " ^ dot_name ^ " > " ^ pdf_name)


let mk_string ?(b="") ?(e="") ?(sep="") f l =
  b ^ (String.concat sep (List.map f l)) ^ e


let dump_file filename = Print.print_integer_ast (parse_file filename)


let localize pos ?(f=pos.Lexing.pos_fname) () =
  let open Lexing in
  let open Printf in
  let car_pos = pos.pos_cnum - pos.pos_bol + 1 in
    printf "fichier %s : ligne %d, caractères %d-%d:\n" f pos.pos_lnum (car_pos-1) car_pos

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
  | Device_bad_input_wire s -> printf "Les fils en entrées du device %s %s" s
      "ne sont pas de la bonne taille ou pas en quantité sufisante" 
  | Bad_block_definition (s,il,ex) -> printf "problème lors de la définition du block %s %s\n" s
      (mk_string ~b:"< " ~e:" >" ~sep:", " string_of_int il) ; analyse_exception ex
  | Undefined_start_block -> printf "Il n'y a pas de point de départ. 
                  Penser à mettre l'instruction start nomBlock dans un des fichiers\n" 
  | Bad_size_enable (bt, var_name) -> printf "à l'instanciation de %s de type de bloc %s\n"
      (block_type bt) var_name
  | e -> raise e


let main filename =
  let open Printf in
  let chan = open_in filename in
  let buf = Lexing.from_channel chan in
    try
      let ast = Parser.circuit Lexer.token buf in
      let analized_ast = SemanticAnalysis.analyse_circuit ast in
	analized_ast
    with 
	Lexer.Lexing_error c ->
	  localize (Lexing.lexeme_start_p buf) ();
	  printf "Erreur dans l'analyse lexicale: %s." c;
	  exit 1
      | Parser.Error ->
	  localize (Lexing.lexeme_start_p buf) ();
	  printf "Erreur dans l'analyse syntaxique.";
	  exit 1
      | e -> analyse_exception e ; failwith ""
