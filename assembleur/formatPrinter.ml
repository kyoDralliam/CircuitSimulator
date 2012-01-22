open Format
open CommonPrinter


let program ppf (text_instruction_list, data_instruction_list) =
  fprintf ppf "%a%a"
    (concat_list "" pp_print_char) text_instruction_list
    (concat_list "" pp_print_char) data_instruction_list

let output_program filename (x,y) = 
  let chan = open_out_bin filename in
  let f = output_char chan in
    List.iter f x ;
    List.iter f y ;
    close_out chan
