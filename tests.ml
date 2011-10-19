let try_option f x = try Some (f x) with _ -> None

module TestPattern =
struct

  open Pattern

  let s = [ "3*n+4" (* accepté *) ; 
	    "k + 7" (* accepté *) ;
	    " k + j + 5" ; (* non accepté *)
	    "a*b"   (* non accepté *) ; 
	    "u + 6 * v + 42" (* accepté *) ;
	    "(u + 5) * (v + 6)" (* non accepté *) ;
	    "42 * ( 2*g + h + 7)" (* non accepté *) ;
	    "2*a + b" (* accepté *) ;
	    "3 + 2*(5+4+8)" (* accepté *) ]
  let ints = List.map Tools.parse_integer s
  let c = List.map (try_option to_combo) ints
  let res = List.map (try_option to_pattern) ints

end 


module TestIntegerToInt =
struct
  open IntegerToInt
  open IntegerToInt
  
  let myMap = 
    let values = [ ("n", 2) ; ("k", 5) ; ("j" , 7) ; ("a", 4) ; ("b", 6) ; ("u", 7) ; ("v", 12) ; ("h" , 9) ] in
      List.fold_left (fun map (x,y) -> StringMap.add x y map) StringMap.empty values

  let ints = List.map (try_option (integer myMap)) TestPattern.ints
end
