open Ast
open IntAst





(**     Prototype pour un device
 *  un device étant par définition 
 *  un bloc dont la définition est 
 *  externe ce bloc ne sert à rien si  
 *  ce n'est à faire office de substitut
 *  à certains endroit 
 *) 

let device_prototype_ = {
  name = "DevicePrototype" ; 
  parameters = [] ; (* peut varier *)
  inputs = [
    "data", 32 ;
    "adress", 32 ;
    "byte_enable", 4 ;
    "write_enable", 1 ;
    "interrupt_enable", 1 ;
    "interrupt_processed", 1
  ] ;
  instantiations = [] ;
  outputs = [ 
    ("data",32), Named_Wire (None,""); 
    ("interrupt",1), Named_Wire (None, "")]
}

let device_prototype_input_number = List.fold_left (fun acc x -> acc + (snd x)) 0 device_prototype_.inputs



(**  Définitions des portes de bases
 *
 *
 *) 

let ground_ = { 
  name = "Gnd" ;
  parameters = [] ;
  inputs = [] ;
  instantiations = [] ;
  outputs = [ ("o", 1), (Named_Wire (None,""))] 
}

let out_ = {
  name = "Out"; 
  parameters = []; 
  inputs = [("a", 1)];
  instantiations = []; 
  outputs = []
}

let in_ = {
  name = "In"; 
  parameters = []; 
  inputs = []; 
  instantiations = [];
  outputs = [(("o", 1), Named_Wire (None, ""))]
}

let vdd_ = {
  name = "Vdd"; 
  parameters = []; 
  inputs = []; 
  instantiations = [];
  outputs = [(("o", 1), Named_Wire (None, "a"))]
}

let xor_ = { 
  name = "Xor" ; 
  parameters = [] ; 
  inputs = [ "a", 1 ; "b", 1] ;  
  instantiations = [] ; 
  outputs = [ ("o", 1), (Named_Wire (None,"")) ]  
}

let and_ = { 
  name = "And" ; 
  parameters = [] ; 
  inputs = [ "a", 1 ; "b", 1] ;  
  instantiations = [] ; 
  outputs = [ ("o", 1), (Named_Wire (None,"")) ]  
}

let or_ = { 
  name = "Or" ; 
  parameters = [] ; 
  inputs = [ "a", 1 ; "b", 1] ;  
  instantiations = [] ; 
  outputs = [ ("o", 1), (Named_Wire (None,"")) ]  
}

let mux_ = { 
  name = "Mux" ; 
  parameters = [] ; 
  inputs = [ "selector", 1 ; "a", 1 ; "b", 1 ] ;  
  instantiations = [] ; 
  outputs = [ ("o", 1), (Named_Wire (None,"")) ]   
}

let reg_ = { 
  name = "Reg" ; 
  parameters = [] ; 
  inputs = [ "a", 1 ] ;  
  instantiations = [] ; 
  outputs = [ ("o", 1), (Named_Wire (None,"")) ]  
}

let not_ = { 
  name = "Not" ; 
  parameters = [] ; 
  inputs = [ "a", 1 ] ;  
  instantiations = [] ; 
  outputs = [ ("o", 1), (Named_Wire (None,"")) ]  
}

(** Blocks de base : plutôt essayer d'aller 
    les chercher dans un autre fichier 
*)
let base_block = [ xor_ ; and_ ; or_ ; mux_ ; reg_ ; not_ ; ground_ ; vdd_ ]

let block_without_loop = [ xor_ ; and_ ; or_ ; mux_ ; not_ ; ground_ ; vdd_ ] 

module BlockType =
struct 
  type t = IntAst.block_type 
  let compare = compare 
end

(** Map ayant comme clés des block_type et 
    comme valeur des block_type_definition *)
module ConcreteBlockMap = Map.Make(BlockType)

(** Set ayant comme clés des block_type *)
module BlockTypeSet = Set.Make(BlockType)
