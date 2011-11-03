open Tools

let _ =
  if Array.length Sys.argv > 1 
  then Print.print_int_ast (main Sys.argv.(1))
