module StringMap = Map.Make(struct type t = string let compare = Pervasives.compare end)

(** Fabrique une 'a StringMap.t à partir d'une (string * 'a) list *)
let create_map l = 
  List.fold_left (fun map (k,v) -> StringMap.add k v map) StringMap.empty l 

type constant = Int of int32 | Char of char 

exception Not_a_register of string
exception Invalid_param of ParseAst.Text.arg * ParseAst.Text.instruction 
exception Invalid_instruction of ParseAst.Text.instruction 
exception Data_bad_specification of ParseAst.Data.instruction
exception Label_error of string
exception Label_not_defined of string
exception Integer_too_big of int32 * int32

type data_instr = 
    Asciiz of string 
  | Ascii of string
  | Word of constant list
  | Space of int
  | Half of constant list
  | Byte of constant list

module T = ParseAst.Text
module D = ParseAst.Data


let ( lsl ) = Int32.shift_left 
let ( lsr ) = Int32.shift_right_logical
let ( + ) = Int32.add
let ( / ) = Int32.div
let ( % ) = Int32.rem
let ( * ) = Int32.mul
let ( ||| ) = Int32.logor
let ( & ) f x = f x 

let chr n = 
  assert ( n < 256l && 0l <= n ) ;
  Char.chr & Int32.to_int n

(* Les fonctions suivantes inversent l'odre des caractères *)
let char_list_of_string s = 
  let res = ref [] in 
    String.iter (fun c -> res := c :: !res ) s ;
    !res 

let int32_to_word x = 
  let sg, x' = if x < 0l then 1l lsl 7, Int32.neg x else 0l, x in 
  let x0 = ( ( x' lsl 0 ) lsr 24 ) ||| sg in
  let x1 = ( x' lsl 8 ) lsr 24 in
  let x2 = ( x' lsl 16) lsr 24 in
  let x3 = ( x' lsl 24) lsr 24 in
    List.map chr [ x3 ; x2 ; x1 ; x0 ]

(*  
  let shift = [| 1l lsl 24 ; 1l lsl 16 ; 1l lsl 8 |] in
  let y, sg = Int32.abs x, if x < 0l then 1l lsl 7 else 0l in  
  let x0 = y / shift.(0) + sg in
  let x1 = (y % shift.(0)) / shift.(1) in
  let x2 = (y % shift.(1)) / shift.(2) in
  let x3 = y % shift.(2) in
    List.map chr [ x3 ; x2 ; x1 ; x0 ]
*)

let char_to_word c =  
  let c0 = chr 0l in
    [ c ; c0 ; c0 ; c0 ]

let int32_to_u_half x = 
  let _ = assert (x >= 0l && x < 1l lsl 16) in
  let x0 = x lsr 8 in
  let x1 = (x lsl 24) lsr 24 in
    List.map chr [ x1 ; x0 ]
  (* let shift = 1l lsl 8 in
  let x0 = x / shift in
  let x1 = x % shift in
    List.map chr [ x1 ; x0 ] *)

let int32_to_half x =  
  let sgn, x' = if x < 0l then 1l lsl 7, Int32.neg x else 0l, x in
  let _ = assert ( x' < 1l lsl 15 ) in
  let x0 = ( (x' lsl 16) lsr 24 ) ||| sgn in
  let x1 = ( x' lsl 24 ) lsr 24 in
    List.map chr [ x1 ; x0 ]

let char_to_half c = 
  let c0 = chr 0l in
    [ c ; c0 ]

let int32_to_u_byte x =
  let _ = assert ( x >= 0l && x < 1l lsl 8 ) in
    chr x

let int32_to_byte x =
  let sgn, x' = if x < 0l then 1l lsl 7, Int32.neg x else 0l, x in
  let _ = assert ( x' < 1l lsl 7 ) in
  let x0 = x' + sgn in
    chr x0


let accumulate a = 
  let iter_fun i x = 
    match i with
      | 0 -> () 
      | i -> a.(i) <- Pervasives.(+) a.(i-1) x
  in
    Array.iteri iter_fun a ; a


(** gestion des registres *)

let get_reg =
  let registers = 
    create_map
      [
	"zero", 31l ; "at", 1l  ; "v0", 2l  ; "v1", 3l  ;
	"a0", 4l   ; "a1", 5l  ; "a2", 6l  ; "a3", 7l  ;
	"t0", 8l   ; "t1", 9l  ; "t2", 10l ; "t3", 11l ;
	"t4", 12l  ; "t5", 13l ; "t6", 14l ; "t7", 15l ;
	"s0", 16l  ; "s1", 17l ; "s2", 18l ; "s3", 19l ;
	"s4", 20l  ; "s5", 21l ; "s6", 22l ; "s7", 23l ;
	"t8", 24l  ; "t9", 25l ; "k0", 26l ; "k1", 27l ;
	"gp", 28l  ; "sp", 29l ; "fp", 30l ; "ra", 0l
      ]
  in
    fun s ->
      try StringMap.find s registers
      with Not_found -> raise (Not_a_register s)
	
let int_with_modifier n = function
  | T.All -> n
  | T.Up -> n lsr 16
  | T.Down -> (n lsl 16) lsr 16
