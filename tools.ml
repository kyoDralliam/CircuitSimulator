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
  let print_parameter = function  
      Parameter_Name s -> printf "%s, " s  
    | Parameter_Value i -> printf "%i, " i
  in
  let print_input (s,i) = printf "%s -> %s, " s (string_of_integer i) in
  let print_instanciation inst = printf "%s, " (fst inst.block_type) in
  let print_output o = printf "%s, " (fst (fst o)) in
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
