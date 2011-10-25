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
  let circuit = parse_file filename in
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
    List.iter print_block (snd circuit) 

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

end
