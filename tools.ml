let lex_file filename =
  let chan = open_in filename in
  let t = Lexing.from_channel chan in
  let rec process acc =
    match Lexer.token t with
      | Parser.EOF -> List.rev acc
      | x -> process (x::acc)
  in process []

let parse_file filename = 
  let chan = open_in filename in
  let res = Parser.circuit Lexer.token (Lexing.from_channel chan) in
    res
