open CommonFormat

type part = RS | RT | RD | SHAMT

type t = 
    {
      rs : int32 ;
      rt : int32 ;
      rd : int32 ;
      shamt : int32 ;
      funct : int32 
    }
      
let empty = 
  {
    rs = 31l ;
    rt = 0l ;
    rd = 0l ;
    shamt = 0l ;
    funct = 0l
  }
    
let map = 
  create_map
    [
      "and", (0b000000l, [ RD ; RS ; RT ] ) ;
      "or",  (0b000001l, [ RD ; RS ; RT ] ) ;
      "xor", (0b000010l, [ RD ; RS ; RT ] ) ;
      "nand",(0b000011l, [ RD ; RS ; RT ] ) ;
      "nor", (0b000100l, [ RD ; RS ; RT ] ) ;
      "add", (0b000101l, [ RD ; RS ; RT ] ) ;  
      "sub", (0b000110l, [ RD ; RS ; RT ] ) ;
      (* "mul", (0b000111l, [ RD ; RS ; RT ] ) ; *)
      "seq", (0b001000l, [ RD ; RS ; RT ] ) ;
      "sne", (0b001000l, [ RD ; RS ; RT ] ) ;
      "slt", (0b001010l, [ RD ; RS ; RT ] ) ;
      "sgt", (0b001011l, [ RD ; RS ; RT ] ) ;
      "sle", (0b001100l, [ RD ; RS ; RT ] ) ;
      "sge", (0b001000l, [ RD ; RS ; RT ] ) ;
      (* quels shifts faut-il garder ??? *)
      "srl", (0b001111l, [ RD ; RT ; SHAMT ] ) ;
      "srlv", (0b001111l, [ RD ; RT ; RS ] ) ;
      (*
	"sll", (0b111100l, [ RD ; RT ; SHAMT ] ) ;
	"sllv", (0b111100l, [ RD ; RT ; RS ] ) ;
	"sra", (0b111100l, [ RD ; RT ; SHAMT ] ) ; 
	"srav",(0b111100l, [ RD ; RT ; RS ] ) ;    
	"sllv",(0b111100l, [ RD ; RT ; RS ] ) 
      *)
    ]

let parse n l (funct, args) =  
  let aux acc x y = 
    match (x,y) with
      | T.Reg s, RD -> { acc with rd = get_reg s }
      | T.Reg s, RS -> { acc with rs = get_reg s }
      | T.Reg s, RT -> { acc with rt = get_reg s }
      | T.Int (n, _), SHAMT -> 
	  if n < 32l 
	  then { acc with shamt = n } 
	  else raise (Integer_too_big (n, 32l) )
      | x, _ -> raise (Invalid_param (x, T.Instruction (n,l)))
  in
  let format = { empty with funct = funct } in
    try
      List.fold_left2 aux format l args
    with Invalid_argument _ -> raise & Invalid_instruction ( T.Instruction (n, l) )

let shift = accumulate [| 0 ; 6 ; 5 ; 5 ; 5 ; 5 ; 6 |]
  
let to_char_list x = 
  let res = 
    x.funct lsl shift.(4) +
      x.shamt lsl shift.(3) +
      x.rd lsl shift.(2) +
      x.rt lsl shift.(1) +
      x.rs lsl shift.(0)
  in
    int32_to_word res
