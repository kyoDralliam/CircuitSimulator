open Format
open CommonPrinter

let localize pos =
  let open Lexing in
  let open Printf in
  let car_pos = pos.pos_cnum - pos.pos_bol + 1 in
    printf "fichier %s : ligne %d, caractères %d-%d:\n" 
      pos.pos_fname pos.pos_lnum (car_pos-1) car_pos


let not_src_file () = 
  Printf.printf "Some source files are needed" ; exit 1

let lex_error lexbuf e = 
  printf "Erreur dans l'analyse lexicale :\n" ;
  CommonPrinter.print_location std_formatter (Loc.location_from_lexbuf lexbuf) ;
  let _ = 
    match e with
      | Lexer.Lexing_error s -> printf "Unknown character : %s\n" s
      | Lexer.EOF_in_string s -> printf "EOF found while lexing a string : %s\n" s
      | e -> (printf "Unknown error.\n" ; raise e)
  in
    exit 1 

let parse_error lexbuf e = 
  printf "Erreur dans l'analyse syntaxique :\n" ;
  CommonPrinter.print_location std_formatter (Loc.location_from_lexbuf lexbuf) ;
  let _ = 
    match e with
(*      | Parser.Parse_error -> printf "Unknown character : %s\n" printf s *)
      | e ->  ( printf "Unknown error.\n" ; raise e )
  in
    exit 1 

let link_error e = 
  printf "Erreur dans le reste du programme :\n" ;
  let open CommonFormat in
  let _ = 
    match e with
      | Not_a_register s -> printf "$%s does not name a register.\n" s

      | Invalid_param (arg, instr) -> 
	  printf "Invalid parameter %a in instruction %a.\n"
	    ParseAstPrinter.TextPrinter.arg arg
	    ParseAstPrinter.TextPrinter.instruction instr

      | Invalid_instruction instr -> 
	  printf "The following instruction is invalid : %a.\n"
	    ParseAstPrinter.TextPrinter.instruction instr

      | Data_bad_specification instr ->
	  printf "Incorrect instruction in the data segment : %a.\n"
	    ParseAstPrinter.DataPrinter.instruction instr

      | Label_error s ->
	  printf "The label %s is defined twice.\n" s
 
      | Label_not_defined s ->
	  printf "The label %s is not defined.\n" s

      | Integer_too_big (n, n0) -> 
	  printf "The integer %ld should be < %ld.\n" n n0
      | e -> printf "...\n" ; raise e
  in
    exit 1
