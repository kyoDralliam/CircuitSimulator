open CommonFormat

type address = Const of int32 | Label of string

type t =
    {
      opcode : int32 ;
      address : address
    }

      
let map = 
  create_map
    [
      "j", 0b101111l ;
    ] 
    
let parse n l opcode = 
  let ad = 
    match l with
      | [ T.Int (n, _) ] -> Const n
      | [ T.Lab (l, _) ] -> Label l
      | _ -> raise & Invalid_instruction ( T.Instruction (n, l) )
  in
    { opcode = opcode ; address = ad }
      
let shift = accumulate [| 0 ; 6 ; 26 |]
  
let to_char_list x =
  let ad = match x.address with Const t -> t | Label _ -> assert false in
  let res = 
    ad lsl shift.(1) +
      x.opcode lsl shift.(0)
  in
     int32_to_word res
