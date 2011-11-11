open Tools
open Arg


let setup_arg_parsing () =
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
    (!output_lex, 
     !output_parse, 
     !output_analyse, 
     !output_graph, 
     !output_graph_pdf,   
     !output_simulator, 
     !output_simulatorV2, 
     !output_c, 
     !output_o,
     !sources)
    

let lex_source source_content output_lex =
  let lexbuf = Lexing.from_string source_content in
    begin
      if output_lex <> "" 
      then 
	let lex_result = 
	  let rec process acc =
	    try
	      match Lexer.token lexbuf with
		| Parser.EOF -> List.rev acc
		| x -> process (x::acc)
	    with 
		Lexer.Lexing_error c ->
		  localize (Lexing.lexeme_start_p lexbuf);
		  Printf.printf "Erreur dans l'analyse lexicale: %s." c;
		  exit 1
	  in process [] 
	in
	let tokens = mk_string ~b:"[" ~e:"]" ~sep:", " 
	  Print.LexerPrinter.token lex_result in
	  output_to_file output_lex tokens 
    end;
    Lexing.from_string source_content


let parse_lexbuf lexbuf output_parse =
    try
      let ast = Parser.circuit Lexer.token lexbuf in
	begin
	  if output_parse <> "" 
	  then Printf.fprintf (open_out output_parse) 
	    "%s" (Print.integer_ast_to_string ast)
	end;
	ast
    with 
	Parser.Error ->
	  localize (Lexing.lexeme_start_p lexbuf);
	  Printf.printf "Erreur dans l'analyse syntaxique.";
	  exit 2


let analyse_ast ast output_analyse = 
  try
    let analysed_ast = SemanticAnalysis.analyse_circuit ast in	  
      begin
	if output_analyse <> ""
	then Printf.fprintf (open_out output_analyse) 
	  "%s" (Print.int_ast_to_string analysed_ast)
      end;
      analysed_ast
  with e -> analyse_exception e ; exit 3

let create_graph analysed_ast output_graph output_graph_pdf =
  let graph = AstToGraph.main analysed_ast in
    begin
      if output_graph <> ""
      then output_to_file output_graph (Print.GraphPrinter.graph graph)
    end;
    begin 
      if output_graph_pdf <> ""
      then ignore (mk_pdf output_graph_pdf (Print.GraphPrinter.graph graph))
    end;
    graph

(* FIXME : à implémenter *)
let create_simulator graph output_simulator = ()

(* FIXME : à implémenter *)
let create_simulatorV2 graph output_simulatorV2 = ()

(* FIXME : à implémenter *)
let create_c_source graph output_c = ()

(* FIXME : à implémenter *)
let create_executable graph output_o = ()


let _ =

  let (output_lex, 
       output_parse, 
       output_analyse, 
       output_graph, 
       output_graph_pdf,   
       output_simulator, 
       output_simulatorV2, 
       output_c, 
       output_o,
       sources) = setup_arg_parsing () in

    let source_content = get_files_content sources in
    let lexbuf = lex_source source_content output_lex in
    let ast = parse_lexbuf lexbuf output_parse in
    let analised_ast = analyse_ast ast output_analyse in
    let graph = create_graph analised_ast output_graph output_graph_pdf in
    let _ = create_simulator graph output_simulator in
    let _ = create_simulatorV2 graph output_simulatorV2 in
    let _ = create_c_source graph output_c in
    let _ = create_executable graph output_o in

      0

	    
