open Format

let output_file filename f x =
  let chan = open_out_bin filename in
    f (formatter_of_out_channel chan) x ;
    close_out chan 

let print f x = f std_formatter x


let write ppf s = fprintf ppf s

let rec concat_list sep f ppf = function
  | [] -> fprintf ppf ""
  | [x] -> fprintf ppf "%a" f x
  | x::xs -> fprintf ppf ("%a%a%a") f x write sep (concat_list sep f) xs

let pair f sep g ppf (x,y) = fprintf ppf "%a%a%a" f x write sep g y

let some f default ppf = function
  | Some x -> f ppf x
  | None -> write ppf default 

let f_some f ppf x = 
  match x with
  | Some x -> f ppf x
  | None -> fprintf ppf ""


let print_location ppf l =
  let open Loc in
  let print_line ppf _ = 
    if l.begin_line = l.end_line
    then fprintf ppf "line %d" l.begin_line
    else fprintf ppf "lines %d-%d" l.begin_line l.end_line 
  in
    fprintf ppf "File \"%s\", %a, characters %d-%d:@\n" l.file 
      print_line () l.begin_character l.end_character

let print_char ppf = fprintf ppf "\'%c\'"
let print_int32 ppf = fprintf ppf "%ld"
