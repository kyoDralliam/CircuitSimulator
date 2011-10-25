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

  let my_ast = Tools.parse_file "../test1"
  let [half_adder ; full_adder ; par_adder_1 ; par_adder_n ] = List.rev (snd my_ast)
  let map_test = StringMap.add "n" 5 (StringMap.empty)
  let res = block_type_definition map_test par_adder_n
end

module TestSemantic =
struct

  let res = SemanticAnalysis.analyse_circuit TestIntegerToInt.my_ast

  let print input = SemanticAnalysis.ConcreteBlockMap.iter 
    (fun k x -> Tools.IntAstPrinter.(print_block_type k ; print_block x)) (snd input)

end
