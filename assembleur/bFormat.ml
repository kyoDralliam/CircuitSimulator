open CommonFormat

type t =
    {
      opcode : int32 ;
      rs : int32 ;
      rt : int32 ;
      label : string * int32 ;
      delta : int32
    }

let empty = 
  {
    opcode = 0l ;
    rs = 0l ;
    rt = 0l ;
    label = "", 0l ;
    delta = 0l ;
  }


let map =
  create_map
    [
      "beq", 0b000110l ;
      "bne", 0b100110l ; 
      "bgt", 0b110110l;
      "bge", 0b101110l;
      "blt", 0b010110l;
      "ble", 0b001110l
    ]

let parse n l opcode = 
  match l with
    | [ T.Reg s ; T.Reg t ; T.Lab (l, _) ] ->
	{ 
	  empty with
	    opcode = opcode ;
	    rs = get_reg s ;
	    rt = get_reg t ;
	    label = l, 0l
	}
    | _ -> raise & Invalid_instruction ( T.Instruction (n, l) )

let shift = accumulate [| 0 ; 5 ; 5 ; 6 |]

let to_char_list x = 
   let res =
     x.rt lsl shift.(0) 
     ||| x.rs lsl shift.(1) 
       ||| x.opcode lsl shift.(2)
   in
     ( List.rev & int32_to_u_half res ) 
     @ ( List.rev & int32_to_half x.delta )
       
