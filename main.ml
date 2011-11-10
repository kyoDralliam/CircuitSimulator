open Tools
open Arg


let _ =
  let output_lex = ref "" in 
  let output_parse = ref "" in
  let output_analyse = ref "" in
  let output_graph = ref "" in
  let output_graph_pdf = ref "" in
  let output_simulator = ref "" in
  let output_simulatorV2 = ref "" in
  let output_c = ref "" in
  let output_o = ref "" in

  let sources = ref [] in
     
  let options = [
    "-lex", Set_string output_lex, "output the result of the lexing in file" ;
    "-parse", Set_string output_parse, "output the result of the parsing in file";
    "-analyse", Set_string output_analyse, "output the result of the semantic analysis in file";
    "-graph", Set_string output_graph, "output the graph in file";
    "-pdf", Set_string output_graph_pdf, "output the pdf resulting from the graph in file";
    "-simulator", Set_string output_simulator, "output the result of compiling as specified in simulator.ml";
    "-simulatorV2", Set_string output_simulatorV2, "output the result of compiling as specified in simulatorV2.ml";
    "-c", Set_string output_c, "output the result of compiling in c";
    "-o", Set_string output_o, "output the result of compiling"
  ] in 
    
  let annon_fun s = sources := s::!sources in
  let usage_msg = "main.native [options] source" in 

    parse options annon_fun usage_msg ;

    let source_content = get_files_content !sources in
    let lexbuf = Lexing.from_string source_content in

      try 

	(* FIXME : échec possible durant cet appel à gérer*)
	(if !output_lex <> "" 
	 then 
	   let tokens = mk_string ~b:"[" ~e:"]" ~sep:", " 
	     Print.LexerPrinter.token (lex_string source_content) in
	     output_to_file !output_lex tokens );
	
	let ast = Parser.circuit Lexer.token lexbuf in
	  (*
	  (if !output_parse <> "" 
	   then Printf.fprintf (open_out !output_parse) "%s" (Print.integer_ast_to_string ast));*) 
	   
	  let analysed_ast = SemanticAnalysis.analyse_circuit ast in
	  
	    (if !output_analyse <> ""
	     then Printf.fprintf (open_out !output_parse) "%s" (Print.int_ast_to_string analysed_ast));
	    
	    let graph = TestDestruction.main analysed_ast in

	      (if !output_graph <> ""
	       then output_to_file !output_graph (Print.GraphPrinter.graph graph));

	      (if !output_graph_pdf <> ""
	       then ignore (mk_pdf !output_graph_pdf (Print.GraphPrinter.graph graph)));

	      ()

      (* FIXME : rajouter simulator, simulatorV2, to_c, to_o, Generationgraphe ... *)

      with 
	  Lexer.Lexing_error c ->
	    localize (Lexing.lexeme_start_p lexbuf);
	    Printf.printf "Erreur dans l'analyse lexicale: %s." c;
	    exit 1
	| Parser.Error ->
	    localize (Lexing.lexeme_start_p lexbuf);
	    Printf.printf "Erreur dans l'analyse syntaxique.";
	    exit 2
	| e -> analyse_exception e ; exit 3
	    
