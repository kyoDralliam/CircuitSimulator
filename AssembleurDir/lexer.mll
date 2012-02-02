{


  open Parser

  exception Lexing_error of string
  exception EOF_in_string of string

  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
      lexbuf.Lexing.lex_curr_p <-
	{ pos with
	    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
	    Lexing.pos_bol = pos.Lexing.pos_cnum;
	}
	
  let current_string = Buffer.create 16
  let ( << ) b c = Buffer.add_char b c ; b

}

let mnemonic = ['a'-'z']+

let register = '$'((['a'-'z']['a'-'z''0'-'9'] | "zero") as r)

let label = ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']+

let newline = '\n' | '\r' | '\r' '\n' | '\n' '\r'

let chiffre = '-'? ( "0b" ['0''1''_']+ | "0x"['0'-'9''a'-'f''A'-'F''_']+ | ['0'-'9''_']+ )

rule token = parse
  | newline                 { incr_linenum lexbuf ; NL }
  | [ ' ' '\t' ]+           { token lexbuf }
  | ".data"                 { DATA }
  | ".text"                 { TEXT } 
  | '.'                     { DOT }
  | register                { REG r }             
  | ':'                     { COLON }
  | '#' [^'\n' '\r']*       {  token lexbuf }
  | ','                     { COMMA }
  | '('                     { LPAREN }  
  | ')'                     { RPAREN } 
  | chiffre as n            { INT (Int32.of_string n) }
  | mnemonic as id          { IDENT id }
  | label as id             { IDENT id }
  | '"'                     { Buffer.clear current_string ; string lexbuf } 
  | '\'' (_ as c) '\''      { CHAR c }
  | eof                     { EOF }
  | _ as c                  { failwith (Printf.sprintf "Caractère inconnu : \"%c\"" c) }

and string = parse 
  | newline              { incr_linenum lexbuf ; string lexbuf }
  | "\\n"                { let _ = current_string << '\\' << 'n' in string lexbuf }
  | "\\\""               { let _ = current_string << '\\' << '"' in string lexbuf }
  | '"'                  { STRING (Buffer.contents current_string) }
  | _  as c              { let _ = current_string << c in string lexbuf } 
  | eof                  { raise (EOF_in_string (Buffer.contents current_string)) } 
