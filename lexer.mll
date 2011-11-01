(**

éléments du lexer :

identifiantMajuscule     UID
identifiantminuscule     LID
start                    START
<                        LESS
>                        GREATER
(                        LPAREN
)                        RPAREN
->                       ARROW
;                        SEMI
:                        COLON
entier                   INT
,                        COMMA
[                        LSQBR
]                        RSQBR
{                        LBRACK
}                        RBRACK
.                        DOT
..                       DOTDOT
+                        PLUS
-                        MINUS
*                        TIMES
/                        DIV
%                        MOD

*)

{
  (* zone de définitions ocaml *)

  open Parser

  exception Lexing_error of string

  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
      lexbuf.Lexing.lex_curr_p <- 
	{ pos with
	    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
	    Lexing.pos_bol = pos.Lexing.pos_cnum;
	}

}

(* définitions de regex *)
let newline = '\n' | '\r' | '\r' '\n' | '\n' '\r'

let alphaNum = ['A'-'Z' 'a'-'z' '_' '0'-'9']
let upperCase = ['A'-'Z']
let lowerCase = ['a'-'z']
let int = ['0'-'9' '_'] + 
(* si on veut rajouter les hexadécimaux, octals ou binaires
   ( extrait de l'exemple de cédric pasteur)
   | '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
   | '0' ['o' 'O'] ['0'-'7']+
   | '0' ['b' 'B'] ['0'-'1']+
*)

rule token = parse
  | newline                 { incr_linenum lexbuf ; token lexbuf }
  | [ ' ' '\t' ] +          { token lexbuf }
  | "start"                 { START }
  | "device"                { DEVICE }
  | "<"                     { LESS }
  | ">"                     { GREATER } 
  | "("                     { LPAREN }
  | ")"                     { RPAREN }
  | "->"                    { ARROW }
  | ";"                     { SEMI }
  | ":"                     { COLON }
  | int                     { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | upperCase alphaNum*     { UID (Lexing.lexeme lexbuf) }
  | lowerCase alphaNum*     { LID (Lexing.lexeme lexbuf) }
  | ","                     { COMMA }
  | "["                     { LSQBR }
  | "]"                     { RSQBR }
  | "{"                     { LBRACK }
  | "}"                     { RBRACK }
  | "."                     { DOT }
  | ".."                    { DOTDOT }
  | "+"                     { PLUS }
  | "-"                     { MINUS }
  | "*"                     { TIMES }
  | "/"                     { DIV }
  | "%"                     { MOD }
  | "^"                     { POWER }  
  | eof                     { EOF }  
  | _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }
