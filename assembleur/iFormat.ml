open CommonFormat

type part = RS | RT | Imm | Ad
    
type pseudo_constant = Const of constant | Label of string

type t =
    { 
      opcode : int32 ;
      rs : int32 ;
      rt : int32 ;
      immediate : pseudo_constant 
    }	
      
let empty = 
  {
    opcode = 0l ;
    rs = 0l ;
    rt = 0l ;
    immediate = Const (Int 0l) ;
  }

let map = 
  create_map
    [
      "andi", (0b000011l, [ RT ; RS ; Imm ] ) ;
      "ori",  (0b100011l, [ RT ; RS ; Imm ] ) ;
      "xori", (0b010011l, [ RT ; RS ; Imm ] ) ;
      "nori", (0b001011l, [ RT ; RS ; Imm ] ) ;
      "addi", (0b101011l, [ RT ; RS ; Imm ] ) ;
      "subi", (0b011011l, [ RT ; RS ; Imm ] ) ;
      "beq",  (0b111111l, [ RS ; RT ; Imm ] ) ;
      "lw",   (0b110001l, [ RS ; Ad ] ) ; 
      "sw",   (0b110101l, [ RS ; Ad ] ) ; 
      "bne",  (0b01l, [ RS ; RT ; Imm ] ) ; (* à corriger *)
      "lb",   (0b01l, [ RS ; Ad ] ) ; 
      "lh",   (0b01l, [ RS ; Ad ] ) ;
      "sh",   (0b01l, [ RS ; Ad ] ) ;  
      "sb",   (0b01l, [ RS ; Ad ] ) ; 
      "lui",  (0b01l, [ RT ; Imm ]) ; 
      (* slti ? *)
    ]
    
let parse n l (opcode, args) = 
  let aux acc x y =
    match (x,y) with
      | T.Reg s, RS -> { acc with rs = get_reg s }
      | T.Reg s, RT -> { acc with rt = get_reg s }
      | T.Int n, Imm -> { acc with immediate = Const (Int n) }
      | T.Char c, Imm -> { acc with immediate = Const (Char c) }
      | T.Lab s, Imm -> { acc with immediate = Label s }
      | T.Shift (n, s), Ad -> 
	  if n > 1l lsl 16 then raise & Integer_too_big (n, 1l lsl 16) ;
	  { acc with rs = get_reg s ; immediate = Const (Int n) } 
      | x, _ -> raise (Invalid_param (x, T.Instruction (n,l)))
  in
  let format = { empty with opcode = opcode } in
    try 
      List.fold_left2 aux format l args
    with Invalid_argument _ -> raise & Invalid_instruction ( T.Instruction (n, l) )
      
let shift = accumulate [| 0 ; 5 ; 5 ; 6 |]
  
let to_char_list x = 
  let res =
    x.rt lsl shift.(0) +
      x.rs lsl shift.(1) +
      x.opcode lsl shift.(2)
  in
  let imm = 
    match x.immediate with
      | Const (Int n) -> 
	  if n < 1l lsl 16
	  then int32_to_half n
	  else raise ( Integer_too_big (n, 1l lsl 16) )
      | Const (Char c) -> 
	  char_to_half c
      | Label _ -> assert false
  in
    ( List.rev & int32_to_half res ) @ ( List.rev imm )
      
