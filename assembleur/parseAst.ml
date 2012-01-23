module Text =
struct

  type modifier = All | Up | Down

  type arg = 
      Reg of string
    | Lab of string * modifier
    | Shift of int32 * string
    | Int of int32 * modifier
    | Char of char 

  type instruction = 
      Label of string 
    | Instruction of string * arg list

end


module Data =
struct

  type arg =
      String of string
    | Char of char list
    | Int of int32 list

  type instruction = 
      Label of string
    | Instruction of string * arg

end

type section = 
  | TextSection of Text.instruction list
  | DataSection of Data.instruction list

type program = Text.instruction list * Data.instruction list

let mk_program l = 
  let split_section = function 
    | TextSection _ -> true
    | DataSection _ -> false
  in
  let open List in
  let text_l, data_l = partition split_section l in
    (
      concat (map (function (TextSection l) -> l | _ -> assert false ) text_l),
      concat (map (function (DataSection l) -> l | _ -> assert false ) data_l)
    )

