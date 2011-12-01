open Tools
open Arg

(** Graph1 -> graphe de Damien 
    Graph2 -> graphe de Kenji
 *)
type graph_style = Graph1 | Graph2

type graph_type = 
    Graph1Type of Graph1.Typesgraphe.circuit 
  | Graph2Type of Graph2.AstToGraph.circuit

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
  let graph_style = ref Graph2 in 
  let cc = ref "" in
  let label = ref false in

  let sources = ref [] in
     
  let options = [
    "-lex", Set_string output_lex, "file   output the result of the lexing in file" ;
    "-parse", Set_string output_parse, "file   output the result of the parsing in file";
    "-analyse", Set_string output_analyse, "file   output the result of the semantic analysis in file";
    "-graph", Set_string output_graph, "file   output the graph in file";
    "-pdf", Set_string output_graph_pdf, "file   output the pdf resulting from the graph in file";
    "-label", Set label, "switch on the label on the pdf's arrows";
    "-simulator", Set_string output_simulator, "file   output the result of compiling as specified in simulator.ml in file";
    "-simulatorV2", Set_string output_simulatorV2, "file   output the result of compiling as specified in simulatorV2.ml in file";
    "-c", Set_string output_c, "file   output the result of compiling in c in file";
    "-o", Set_string output_o, "file   output the result of compiling in file" ;
    "-graph1", Unit (fun () -> graph_style := Graph1), "set the graph setting to graph1 (Damien)" ;
    "-graph2", Unit (fun () -> graph_style := Graph2), "set the graph setting to graph2 (Kenji)" ;
    "-cc", Set_string cc, "val   set the c compiler to val (default $(cc) or, if not found, gcc)" ;
    "-susucre", Set_int (ref 16), "n   set the number of sugar put in your tea or coffee"
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
     !sources,
     !graph_style,
     !cc,
     !label)
    

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

(* FIXME : implémenter Graph1 *)
let create_graph graph_style analysed_ast output_graph output_graph_pdf label =
  match graph_style with
    | Graph1 -> Graph1Type ([||],[],[],[]) 
    | Graph2 -> 
	let (g,_) as graph = Graph2.AstToGraph.main analysed_ast in
	  begin
	    if output_graph <> ""
	    then output_to_file output_graph (Print.GraphPrinter.graph g)
	  end;
	  begin 
	    if output_graph_pdf <> ""
	    then 
	      let graph_string = Print.GraphPrinter.graph ~label g in
		ignore (mk_pdf output_graph_pdf graph_string)
	  end;
	  Graph2Type graph

(* FIXME : implémenter Graph1 *)
let create_simulator graph output_simulator = 
  if output_simulator <> ""
  then 
    match graph with
      | Graph1Type g -> 
	  output_to_file output_simulator (Graph1.ToSimulator.main g)
      | Graph2Type (g,_) -> 
	  output_to_file output_simulator (Graph2.ToSimulator.main g)

(* FIXME : implémenter Graph1 *)
let create_simulatorV2 graph output_simulatorV2 = 
  if output_simulatorV2 <> ""
  then 
    match graph with
      | Graph1Type g -> 
	  output_to_file output_simulatorV2 (Graph1.ToSimulatorV2.string_of_graphe g)
      | Graph2Type (g,_) -> 
	  output_to_file output_simulatorV2 (Graph2.ToSimulatorV2.string_of_graphe g)

let create_cpp_source graph output_cpp = 
  let cpp_source = 
    match graph with
      | Graph1Type g -> failwith "Graph1.GraphToCpp.circuit_code not implemented"
          (* FIXME : Graph1.GraphToCpp.circuit_code g *) 
      | Graph2Type g -> Graph2.GraphToCpp.circuit_code g
  in
  let output_cpp = if output_cpp <> "" then output_cpp else Filename.temp_file "output_cpp" ".cpp" in
    output_buffer_to_file output_cpp cpp_source ;
    output_cpp


(** emploie le module Command tiré d'Ocamlbuild *)
let create_executable cpp_source_file output_o cc = 
  if output_o <> ""
  then 
    let open Ocamlbuild_plugin.Command in
    let cc = 
      if cc <> ""
      then cc
      else 
	try 
	  Sys.getenv "cc"
	with Not_found -> "g++" 
    in 
    let compile = S[ A cc ; A "-o" ; A output_o ; A cpp_source_file ] in
      Sys.command (string_of_command_spec compile)
  else 0

let _ =

  let (output_lex, 
       output_parse, 
       output_analyse, 
       output_graph, 
       output_graph_pdf,   
       output_simulator, 
       output_simulatorV2, 
       output_cpp, 
       output_o,
       sources,
       graph_style,
       cc,
       label) = setup_arg_parsing () in

    let source_content = get_files_content sources in
    let lexbuf = lex_source source_content output_lex in
    let ast = parse_lexbuf lexbuf output_parse in
    let analysed_ast = analyse_ast ast output_analyse in
    let graph = create_graph graph_style analysed_ast output_graph output_graph_pdf label in
    let _ = create_simulator graph output_simulator in
    let _ = create_simulatorV2 graph output_simulatorV2 in
    let cpp_source_file = create_cpp_source graph output_cpp in
    let result = create_executable cpp_source_file output_o cc in

      result

	    
