open Format
open CommonPrinter
open ParseAst

module TextPrinter = 
struct
  open Text

  let arg ppf = function
    | Reg s -> fprintf ppf "$%s" s
    | Lab (s, mf) -> pp_print_string ppf s
    | Shift (n, s) -> fprintf ppf "%ld($%s)" n s
    | Int (n, mf) -> print_int32 ppf n
    | Char c -> print_char ppf c

  let instruction ppf = function
    | Label s -> fprintf ppf "%s:" s
    | Instruction (s, l) -> fprintf ppf "\t%s %a" s (concat_list ", " arg) l

end 

module DataPrinter =
struct
  open Data

  let arg ppf = function
    | String s -> fprintf ppf "\"%s\"" s
    | Int l -> concat_list ", " print_int32 ppf l
    | Char l -> concat_list ", " print_char ppf l

  let instruction ppf = function
    | Label s -> fprintf ppf "%s:" s
    | Instruction (s, l) -> fprintf ppf "\t.%s %a" s arg l

end

let program ppf (text_l, data_l) =
  fprintf ppf "\t.text:@\n%a@\n@\n@\n\t.data:@\n%a" 
    (concat_list "@\n" TextPrinter.instruction) text_l
    (concat_list "@\n" DataPrinter.instruction) data_l
