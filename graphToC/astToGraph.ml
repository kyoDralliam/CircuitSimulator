type gate = 
  | Gnd | Vdd
  | Not | Or | And | Xor 
  | Input of int
  | Multiplexer
  | Register 
  | Output of int
  | Device of (string * int list)
