open CommonFormat

type part = RS | RT | Imm | Ad


type pseudo_constant = Const of constant | Label of string * T.modifier


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
      "andi", (0b110000l, [ RT ; RS ; Imm ] ) ;
      "ori",  (0b110001l, [ RT ; RS ; Imm ] ) ;
      "xori", (0b110010l, [ RT ; RS ; Imm ] ) ;
      "nandi",(0b110011l, [ RT ; RS ; Imm ] ) ;
      "nori", (0b110100l, [ RT ; RS ; Imm ] ) ;
      "addi", (0b110101l, [ RT ; RS ; Imm ] ) ;
      "subi", (0b110110l, [ RT ; RS ; Imm ] ) ;
      (* "muli", (0b110111l, [ RD ; RS ; Imm ] ) ; *)
      "lw",   (0b100011l, [ RS ; Ad ] ) ; 
      "sw",   (0b101011l, [ RS ; Ad ] ) ; 
      "jr",   (0b101000l, [ RS ]) ; 
      "lb",   (0b100001l, [ RS ; Ad ] ) ; 
      (* "lh",   (0b01l, [ RS ; Ad ] ) ; *)
      (* "sh",   (0b01l, [ RS ; Ad ] ) ;  *)
      "sb",   (0b101001l, [ RS ; Ad ] ) ; 
      (* "lui",  (0b01l, [ RT ; Imm ]) ; *)
      (* slti ? *)
    ]
    
let parse n l (opcode, args) = 
  let aux acc x y =
    match (x,y) with
      | T.Reg s, RS -> { acc with rs = get_reg s }
      | T.Reg s, RT -> { acc with rt = get_reg s }
      | T.Int (n, mf) , Imm -> 
	  Printf.printf "int %ld\n" n ; 
	  let n' = int_with_modifier n mf in
	  { acc with immediate = Const (Int n') }
      | T.Char c, Imm -> { acc with immediate = Const (Char c) }
      | T.Lab (s, mf), Imm -> { acc with immediate = Label (s, mf) } 
      | T.Shift (n, s), Ad -> 
	  Printf.printf "shift %ld\n" n ; 
	  if n > 1l lsl 16 then raise & Integer_too_big (n, 1l lsl 16) ;
	  { acc with rs = get_reg s ; immediate = Const (Int n) } 
      | x, _ -> raise (Invalid_param (x, T.Instruction (n,l)))
  in
  let format = { empty with opcode = opcode } in
    try 
      List.fold_left2 aux format l args
    with Invalid_argument _ -> raise & Invalid_instruction ( T.Instruction (n, l) )
      
let shift = accumulate [| 0 ; 6; 5 ; 5 |]
  
let to_char_list x = 
  let res =
    x.rt lsl shift.(2) +
      x.rs lsl shift.(1) +
      x.opcode lsl shift.(0)
  in
  let imm = 
    match x.immediate with
      | Const (Int n) -> 
	  if n >= 0l && n < 1l lsl 16 
	  then int32_to_u_half n
	  else
	    if (Int32.abs n) < 1l lsl 15
	    then int32_to_half n
	    else raise ( Integer_too_big (n, 1l lsl 15) )
      | Const (Char c) -> 
	  char_to_half c
      | Label _ -> assert false
  in
     (int32_to_u_half res )  @ imm 
      
