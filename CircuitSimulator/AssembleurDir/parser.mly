%{

  open ParseAst

%}

%token<string> IDENT, REG, STRING
%token<int32> INT
%token COLON, COMMA, LPAREN, RPAREN, DATA, TEXT, EOF, DOT, NL
%token<char> CHAR


%start<ParseAst.program> file

%%

file:
  | NL* l=sections* EOF          { mk_program l }


sections:
  | DATA NL+ l=data_instruction* { DataSection l }
  | TEXT NL+ l=text_instruction* { TextSection l }

data_instruction:
  | n=IDENT COLON NL+                                       { Data.Label n }
  | DOT n=IDENT d=data_arg NL+                              { Data.Instruction (n, d) }

data_arg:
  | s=STRING                                              { Data.String s }
  | l=separated_nonempty_list(COMMA, INT)                 { Data.Int l }
  | l=separated_nonempty_list(COMMA, CHAR)                { Data.Char l } 


text_instruction:
  | n=IDENT COLON NL+                                        { Text.Label n }
  | n=IDENT l=separated_list(COMMA, text_arg) NL+            { Text.Instruction (n, l) }

text_arg:
  | x=REG                                                 { Text.Reg x }
  | l=IDENT                                               { Text.Lab (l, Text.All) }
  | n=INT LPAREN x=REG RPAREN                             { Text.Shift (n, x) }
  | n=INT                                                 { Text.Int (n, Text.All) }
  | c=CHAR                                                { Text.Char c }

%%
