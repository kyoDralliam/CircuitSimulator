open AstToGraph
open GraphToC

(* Un test : *)
let _ =
  let graph = [| ((Input 1), [|[(3,0);(4,0)]|]);
  ((Input 1), [|[(3,1);(4,1)]|]);
  ((Output 3), [||]);
  (Xor, [|[(2,1);(5,0)]|]);
  (And, [|[(2,0)]|]);
  (Register, [|[(2,2)]|]) |]
  in
  let number_of_circuit_inputs = 2 in
  let number_of_circuit_outputs = 1 in
  let number_of_registers = 1 in
  let number_of_devices = 0 in
  print_string (circuit_code (graph, number_of_circuit_inputs, number_of_circuit_outputs,
  number_of_registers, number_of_devices))
    
