open Tools

let _ =
  if Array.length Sys.argv > 1 
  then IntAstPrinter.print (main Sys.argv.(1))
