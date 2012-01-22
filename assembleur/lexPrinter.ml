open Format 
open Parser

let token ppf = function
  | IDENT s -> fprintf ppf "Ident %s" s
  | REG s -> fprintf ppf "Reg %s" s 
  | STRING s -> fprintf ppf "String %s" s
  | INT n -> fprintf ppf "Int %ld" n
  | COLON -> fprintf ppf ":"
  | COMMA -> fprintf ppf ","
  | LPAREN -> fprintf ppf "("
  | RPAREN -> fprintf ppf ")"
  | DATA -> fprintf ppf ".data"
  | TEXT -> fprintf ppf ".text"
  | EOF -> fprintf ppf "eof"
  | DOT -> fprintf ppf "."
  | NL -> fprintf ppf "newline"
  | CHAR c -> fprintf ppf "Char %c" c
