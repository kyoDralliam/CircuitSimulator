(** type d'une position dans un fichier *)
type location = 
    {
      file : string ;
      begin_line : int ;
      begin_character : int ;
      end_line : int ;
      end_character : int ;
    }

(** La position fantome inexistante *)
let ghost =
  {
    file = "_none_" ;
    begin_line = 0 ;
    begin_character = 0 ;
    end_line = 0 ;
    end_character = 0
  }

(** Fonctions d'aide pour la construction et la manipulation de location *)

(* Fabrique une location *)
let mk_location file begin_line begin_character end_line end_character =
  {
    file = file ;
    begin_line = begin_line ;
    begin_character = begin_character ;
    end_line = end_line ;
    end_character = end_character ; 
  }

(* Fusionne deux location *)
let merge begin_loc end_loc =
  assert (begin_loc.file = end_loc.file) ;
  {
    file = begin_loc.file ;
    begin_line = begin_loc.begin_line ;
    begin_character = begin_loc.begin_character ;
    end_line = end_loc.end_line ;
    end_character = end_loc.end_character ;
  }

(* Transforme deux positions du module Lexing en une location *)
let location_from_positions begin_pos end_pos =
  let open Lexing in 
   assert (begin_pos.pos_fname = end_pos.pos_fname) ;
    {
      file = begin_pos.pos_fname ;
      begin_line = begin_pos.pos_lnum ;
      begin_character = begin_pos.pos_cnum - begin_pos.pos_bol ;
      end_line = end_pos.pos_lnum ;
      end_character = end_pos.pos_cnum - end_pos.pos_bol ;
    }

(* Extrait une location depuis un lexbuf *)
let location_from_lexbuf lexbuf = 
  location_from_positions 
    (Lexing.lexeme_start_p lexbuf)
    (Lexing.lexeme_end_p lexbuf)
