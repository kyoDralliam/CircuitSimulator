open Ast
open IntAst

let xor_ = { 
  name = "Xor" ; 
  parameters = [] ; 
  inputs = [ "a", 1 ; "b", 1] ;  
  instantiations = [] ; 
  outputs = [ ("s", 1), (Named_Wire (None,"")) ]  
}

let and_ = { 
  name = "And" ; 
  parameters = [] ; 
  inputs = [ "a", 1 ; "b", 1] ;  
  instantiations = [] ; 
  outputs = [ ("s", 1), (Named_Wire (None,"")) ]  
}

let or_ = { 
  name = "Or" ; 
  parameters = [] ; 
  inputs = [ "a", 1 ; "b", 1] ;  
  instantiations = [] ; 
  outputs = [ ("s", 1), (Named_Wire (None,"")) ]  
}

let mux_ = { 
  name = "Mux" ; 
  parameters = [] ; 
  inputs = [ "a", 1 ; "b", 1 ; "c", 1 ] ;  
  instantiations = [] ; 
  outputs = [ ("s", 1), (Named_Wire (None,"")) ]   
}

let reg_ = { 
  name = "Reg" ; 
  parameters = [] ; 
  inputs = [ "a", 1 ] ;  
  instantiations = [] ; 
  outputs = [ ("s", 1), (Named_Wire (None,"")) ]  
}

let not_ = { 
  name = "Not" ; 
  parameters = [] ; 
  inputs = [ "a", 1 ] ;  
  instantiations = [] ; 
  outputs = [ ("s", 1), (Named_Wire (None,"")) ]  
}

(** Blocks de base : plutôt essayer d'aller 
    les chercher dans un autre fichier 
*)
let base_block = [ xor_ ; and_ ; or_ ; mux_ ; reg_ ; not_ ]

let block_without_loop = [ xor_ ; and_ ; or_ ; mux_ ; not_ ] 
