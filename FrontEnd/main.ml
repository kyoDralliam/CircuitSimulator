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
  let output_o = ref "a.out" in
  let graph_style = ref Graph2 in 
  let cc = ref "g++" in
  let label = ref false in
  let ccflags = ref [] in

  let rock_sources = ref [] in
  let object_files = ref [] in

  let (@:=) l x = l := x::!l in
     
  let options = [
    "-lex", Set_string output_lex, "file   output the result of the lexing in file" ;
    "-parse", Set_string output_parse, "file   output the result of the parsing in file";
    "-analyse", Set_string output_analyse, "file   output the result of the semantic analysis in file";
    "-graph", Set_string output_graph, "file   output the graph in file";
    "-pdf", Set_string output_graph_pdf, "file   output the pdf resulting from the graph in file";
    "-label", Set label, "switch on the label on the pdf's arrows";
    "-simulator", Set_string output_simulator, "file   output the result of compiling as specified in simulator.ml in file";
    "-simulatorV2", Set_string output_simulatorV2, "file   output the result of compiling as specified in simulatorV2.ml in file";
    "-c", Set_string output_c, "file   output the result of compiling in c++ in file";
    "-o", Set_string output_o, "file   output the result of compiling in file" ;
    "-graph1", Unit (fun () -> graph_style := Graph1), "set the graph setting to graph1 (Damien)" ;
    "-graph2", Unit (fun () -> graph_style := Graph2), "set the graph setting to graph2 (Kenji)" ;
    "-cc", Set_string cc, "val   set the c compiler to val (default $(cc) or, if not found, gcc)" ;
    "-ccflags", String (fun s -> ccflags := (Str.split (Str.regexp_string ",") s)), 
    "flag1,flag2,..,flagn    pass these flags to the c++ compiler" ;
    "-susucre", Set_int (ref 16), "n   set the number of sugar put in your tea or coffee"
  ] in 
    
 
  let annon_fun s = 
    if Filename.check_suffix s ".o" 
    then object_files @:= s
    else rock_sources @:= s 
  in
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
     !rock_sources,
     !object_files,
     !graph_style,
     !cc,
     !label,
     !ccflags
    )

let check_sources rock_sources = 
  let filter_fun f = 
    if Sys.file_exists f 
    then true 
    else (Printf.printf "Warning fichier non trouvé : %s \n" f ; false)
  in
  let sources = List.filter filter_fun rock_sources in
    match sources with
      | [] -> Printf.printf "Au moins un fichier valide doit être présent.\n" ; exit 42
      | l -> l

let lex_source rock_sources output_lex =
  let lex_result f lexbuf = 
    let rec process acc =
      try
	match Lexer.token lexbuf with
	  | Parser.EOF -> List.rev acc
	  | x -> process (x::acc)
      with 
	  Lexer.Lexing_error c ->
	    localize (Lexing.lexeme_start_p lexbuf) ~f ();
	    Printf.printf "Erreur dans l'analyse lexicale: %s." c;
	    exit 1
    in process [] 
  in
  let aux src = 
    let content = file_slurp src in
    let lexbuf = Lexing.from_string content in
    let res = lex_result src lexbuf in
      (src, Lexing.from_string content), (src ,res)
  in
  let lexbufs, lex_res = List.split (List.map aux rock_sources) in
    (if output_lex <> "" 
    then 
      let name (filename, lex_content) = 
	let tokens = mk_string ~b:"[" ~e:"]" ~sep:", "
	  Print.LexerPrinter.token lex_content in
	  "fichier " ^ filename ^ ":\n\n" ^ tokens 
      in
	output_to_file output_lex (String.concat "\n\n\n\n" (List.map name lex_res))) ;
    lexbufs


let parse_lexbuf lexbufs output_parse =
  let parse (f, lexbuf) =
    try 
      f, Parser.circuit Lexer.token lexbuf
    with 
	Parser.Error ->
	  localize (Lexing.lexeme_start_p lexbuf) ~f ();
	  Printf.printf "Erreur dans l'analyse syntaxique.";
	  exit 2
  in
  let asts = List.map parse lexbufs in
    begin
      if output_parse <> "" 
      then 
	let name (filename, parse_content) = "(* fichier " ^ filename ^ " *)\n\n" ^ 
	  (Print.integer_ast_to_string parse_content) in
	  output_to_file output_parse (String.concat "\n\n\n\n" (List.map name asts))
    end;
    List.fold_left merge_ast (List.hd asts) (List.tl asts)
    


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
let create_executable cpp_source_file output_o cc object_files ccflags = 
  let cmd = Printf.sprintf "%s -o '%s' '%s' '%s' '%s'" 
    cc  (String.escaped output_o)
    cpp_source_file
    (String.concat "' '" object_files)
    (String.concat "' '" ccflags) in
    Printf.printf "%s" cmd ;
    Sys.command cmd

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
       rock_sources,
       object_files,
       graph_style,
       cc,
       label,
       ccflags
      ) = setup_arg_parsing () in

  let sources = check_sources rock_sources in
  let lexbufs = lex_source sources output_lex in
  let _, ast = parse_lexbuf lexbufs output_parse in
  let analysed_ast = analyse_ast ast output_analyse in
  let graph = create_graph graph_style analysed_ast output_graph output_graph_pdf label in
  let _ = create_simulator graph output_simulator in
  let _ = create_simulatorV2 graph output_simulatorV2 in
  let cpp_source_file = create_cpp_source graph output_cpp in
  let result = create_executable cpp_source_file output_o cc object_files ccflags in

    exit result

	    
