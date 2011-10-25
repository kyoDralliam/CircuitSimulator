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

  let lexing_error pos =
    let open Lexing in
    let soi = string_of_int in
    let car_pos = pos.pos_bol - pos.pos_cnum in
    failwith
      ("Problème au niveau du lexer " ^
	 "fichier " ^ pos.pos_fname ^ " " ^
	 "ligne " ^ (soi pos.pos_lnum) ^ " " ^
	 "caractère " ^ (soi car_pos))

}

(* définitions de regex *)
let newline = '\n' | '\r'

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
  | newline +               { token lexbuf }
  | [ ' ' '\t' ] +          { token lexbuf }
  | "start"                 { START }
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
  | _                       { lexing_error (Lexing.lexeme_start_p lexbuf) }  
 
(* étoffer un peu les erreurs pour simplifier le débogage*)
