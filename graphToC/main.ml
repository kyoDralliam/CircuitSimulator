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

  let (number_of_circuit_inputs, number_of_circuit_outputs,
    number_of_registers, number_of_devices) =
    begin
      let (number_of_circuit_inputs, number_of_circuit_outputs,
      number_of_registers, number_of_devices) = (ref 0, ref 0, ref 0, ref 0)
      in

      Array.iter
        (function (gate,_) -> match gate with
          | Input _ -> incr number_of_circuit_inputs
          | Output _ -> incr number_of_circuit_outputs
          | Register -> incr number_of_registers
          | Device _ -> incr number_of_devices
          | _ -> ())
        graph;
      
      (!number_of_circuit_inputs, !number_of_circuit_outputs,
      !number_of_registers, !number_of_devices)
    end
  in
  
  print_string (circuit_code graph)

    
