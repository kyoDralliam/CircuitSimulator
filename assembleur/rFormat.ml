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
    rs = 0l ;
    rt = 0l ;
    rd = 0l ;
    shamt = 0l ;
    funct = 0l
  }
    
let map = 
  create_map
    [
      "and", (0x0l, [ RD ; RS ; RT ] ) ;
      "or",  (0x8l, [ RD ; RS ; RT ] ) ;
      "xor", (0x4l, [ RD ; RS ; RT ] ) ;
      "nor", (0x2l, [ RD ; RS ; RT ] ) ;
      "add", (0xAl, [ RD ; RS ; RT ] ) ;
      "sub", (0x6l, [ RD ; RS ; RT ] ) ;
      "jr",  (0x1l, [RS]) ;
      "slt", (0x3l, [ RD ; RS ; RT ] ) ;
      "sgt", (0x5l, [ RD ; RS ; RT ] ) ;
      "sll", (0x7l, [ RD ; RT ; SHAMT ] ) ;
      "srl", (0x9l, [ RD ; RT ; SHAMT ] ) ;
      "sra", (0xBl, [ RD ; RT ; SHAMT ] ) ;
      "srav",(0xCl, [ RD ; RT ; RS ] ) ;    
      "sllv",(0xDl, [ RD ; RT ; RS ] ) 
    ]

let parse n l (funct, args) =  
  let aux acc x y = 
    match (x,y) with
      | T.Reg s, RD -> { acc with rd = get_reg s }
      | T.Reg s, RS -> { acc with rs = get_reg s }
      | T.Reg s, RT -> { acc with rt = get_reg s }
      | T.Int n, SHAMT -> 
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
    x.funct lsl shift.(0) +
      x.shamt lsl shift.(1) +
      x.rd lsl shift.(2) +
      x.rt lsl shift.(3) +
      x.rs lsl shift.(4)
  in
    List.rev & int32_to_word res
