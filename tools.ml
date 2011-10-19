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

(** fonction d'aide pour filtrer les erreurs*)
let wire_identifier_to_string = function
  | None, s2 -> s2
  | (Some s1), s2 -> s1 ^"."^s2


let dump_file filename =
  let circuit = parse_file filename in
  let module IntegerAst = Ast.Make(Ast.Integer) in
  let open Ast.Integer in
  let open IntegerAst in
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
