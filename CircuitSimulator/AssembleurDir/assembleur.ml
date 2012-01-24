


let parse_args () = 
  let open Arg in
  let output_file = ref "a.out" in
  let src_files = ref [] in
  let lex_file = ref "" in
  let parse_file = ref "" in
  let parse_only = ref false in
  let specs = 
    [
      "-parse-only", Set parse_only, "\tOnly parses the source file." ;
      "-parse-file", Set_string parse_file, "file\tset the name of the output of the parse operation." ; 
      "-lex-file", Set_string lex_file, "" ;
      "-o", Set_string output_file, "file\tset the name of the output to file." 
    ] in
  let annon_fun s = 
    if Filename.check_suffix s ".s" && Sys.file_exists s
    then src_files := s :: !src_files 
    else Format.printf "Warning : %s doesn't exist or doesn't have the .s extension" s
  in
  let helper_msg = "assembleur options file.s" in  
    Arg.parse specs annon_fun helper_msg ;
    if !output_file = "a.out" && List.length !src_files = 1
    then output_file := (Filename.chop_suffix (List.hd !src_files) ".s") ^ ".out" ;
    (
      !output_file,
      !src_files,
      !parse_file,
      !parse_only,
      !lex_file
    )

let slurp_files l =
  let file_slurp s =
    let buf = Buffer.create 16 in
    let cin = open_in s in
    let rec aux () = 
      let s,b = 
	try input_line cin, true
	with End_of_file -> "", false
      in
	Buffer.add_string buf s ;
	Buffer.add_char buf '\n' ;
	if b then aux () else ()
    in 
      aux () ; 
      close_in cin ;
      s, Buffer.contents buf
  in
    List.map file_slurp l
    
    
let lex srcs lex_filename = 
  let mk_lexbuf (filename, content) = 
    let open Lexing in
    let lexbuf = from_string content in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename } ;
      lexbuf
  in
  let lex_file info = 
    let lexbuf = mk_lexbuf info in
    let rec aux () = 
      match Lexer.token lexbuf with
	| Parser.EOF as x -> [x] 
	| x -> x ::  ( aux () )
    in
    let l = 
      try aux () 
      with e -> Error.lex_error lexbuf e 
    in
      if lex_filename <> ""
      then CommonPrinter.(output_file lex_filename (concat_list " ; " LexPrinter.token) l) ;
      mk_lexbuf info
  in
    List.map lex_file srcs

let parse lex_srcs parse_file = 
  let parse_aux lexbuf = 
    try Parser.file Lexer.token lexbuf
    with e -> Error.parse_error lexbuf e
  in
  let ast = 
    let open List in 
    let l1,l2 = split (map parse_aux lex_srcs) in
      concat l1, concat l2 
  in
  let _ = 
    if parse_file <> ""
    then CommonPrinter.output_file parse_file ParseAstPrinter.program ast 
  in
    ast
    
let link_and_print program output_file = 
  let program' = 
    try MakeFormats.make_all program 
    with e -> Error.link_error e
  in
    FormatPrinter.output_program output_file program'
    (* CommonPrinter.output_file output_file AnalyseAstPrinter.program program' *)
  


let _ = 
  let (
    output_file,
    src_files,
    parse_file,
    parse_only,
    lex_file
  ) = parse_args () in
    
    if List.length src_files = 0
    then Error.not_src_file () ;

    let srcs = slurp_files src_files in
    let lex_result = lex srcs lex_file in
    let parse_result = parse lex_result parse_file in
      
      if not parse_only 
      then link_and_print parse_result output_file
