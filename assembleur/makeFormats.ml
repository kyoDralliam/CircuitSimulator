open CommonFormat

type format = 
    RFormat of RFormat.t 
  | IFormat of IFormat.t
  | JFormat of JFormat.t

type program = char list * char list


type variable = X | Y | Z
type var_or_const = Var of variable | Const of ParseAst.Text.arg
module VarMap = Map.Make(struct type t = variable let compare = Pervasives.compare end)

let pseudo_instructions = 
  let ( ~? ) x = Var x in 
  let ( ~! ) x = Const x in 
  let const_0 = ~!(T.Int 0l) in
  let reg0 = ~!(T.Reg "zero") in
  let at_reg = ~!(T.Reg "at") in
    create_map
      [
	"move", ( [ X ; Y ], [ "addi", [ ~?X ; ~?Y ; const_0 ] ] ) ;
	"clear", ( [ X ], [ "add", [ ~?X ; reg0 ; reg0 ] ] ) ;
	"la", ( [ X ; Y ], [ "lui", [ ~?X ; ~?Y ] ; "addi", [ ~?X ; reg0 ; ~?Y ] ] ) ;
	"li", ( [ X ; Y ], [ "lui", [ ~?X ; ~?Y ] ; "addi", [ ~?X ; reg0 ; ~?Y ] ] ) ;
	"b", ( [ X ], [ "beq", [ reg0 ; reg0 ; ~?X ] ] ) ;
	"bgt", ( [ X ; Y ; Z ], [ "slt", [ at_reg ; ~?Y ; ~?X ] ; "bne", [ at_reg ; reg0 ; ~?Z ] ] ) 
      ]

let get_pseudo s l = 
  let (l0, instrs) = 
    try 
      StringMap.find s pseudo_instructions 
    with Not_found -> 
      assert false 
  in 
  let params = 
    try
      List.fold_left2 (fun map k v -> VarMap.add k v map) VarMap.empty l0 l 
    with Invalid_argument _ ->
      raise & Invalid_instruction ( T.Instruction (s, l) )
  in
  let subst = function
    | Var x -> (try VarMap.find x params with Not_found -> assert false)
    | Const x -> x
  in
  let subst' (s, l) = T.Instruction (s, List.map subst l) in
    List.map subst' instrs


open StringMap 

class text_context = 
object
  val labels = StringMap.empty
  val pc = 0x0l
  val instructions : format list = []
    
  method add_label l = 
    if mem l labels 
    then raise (Label_error l)
    else
      {< 
	labels = add l pc labels
      >}

  method add instr =
    {<
      pc = pc + 4l ;
      instructions = instr :: instructions
    >}

  method result = labels, List.rev instructions

  method end_address = pc
end

let rec text_instruction ctx = function
  | T.Label l -> ctx # add_label l

  | T.Instruction (n, l) when mem n RFormat.map ->
      let res = RFormat.parse n l & try find n RFormat.map with Not_found -> assert false in
	ctx#add (RFormat res)

  | T.Instruction (n, l) when mem n IFormat.map ->
      let res = IFormat.parse n l & try find n IFormat.map with Not_found -> assert false in
	ctx#add (IFormat res)
	
  | T.Instruction (n, l) when mem n JFormat.map ->
      let res = JFormat.parse n l & try find n JFormat.map with Not_found -> assert false in
	ctx#add (JFormat res)

  | T.Instruction (n, l) when mem n pseudo_instructions ->
      List.fold_left text_instruction ctx (get_pseudo n l)

  | x -> raise & Invalid_instruction x


(* 
   memory map :
   afficheur => début 0x800000000 --> label : clock_display
   horloge => début 0x80001000 --> label : timestamp

*)

class data_context begin_address = 
object
  val address = begin_address
  val labels = 
    add "clock_display" 0x80000000l & 
      add "timestamp" 0x80001000l &
      empty

  val data : char list = [] 

  method add_label l = 
    if mem l labels 
    then raise (Label_error l)
    else 
      {<
	labels = add l address labels
      >}

  method add instr = 
    let byte_size = Int32.of_int & List.length instr in
      {<
	address = address + byte_size ;
	data = instr @ data
      >}

  method result = labels, List.rev data
end


let data_instruction ctx = function
  | D.Label l -> ctx # add_label l
  
  | D.Instruction ("asciiz", D.String s) ->
      ctx # add ((chr 0l) :: (char_list_of_string s))

  | D.Instruction ("ascii", D.String s) ->
      ctx # add  (char_list_of_string s)

  | D.Instruction ("word", D.Int l) ->
      ctx # add List.( concat & rev & map int32_to_word l ) 

  | D.Instruction ("word", D.Char l) ->
      ctx # add List.( concat & rev & map char_to_word l )
      
  | D.Instruction ("half", D.Int l) ->
      ctx # add List.( concat & rev & map int32_to_half l ) 

  | D.Instruction ("half", D.Char l) ->
      ctx # add List.( concat & rev & map char_to_half l )

  | D.Instruction ("byte", D.Int l) ->
       ctx # add List.( rev & map int32_to_byte l ) 

  | D.Instruction ("byte", D.Char l) -> 
      ctx # add List.( rev l )

  | D.Instruction ("space", D.Int [n]) ->
      ctx # add Array.( to_list & make (Int32.to_int n) & chr 0l )

  | x -> raise & Data_bad_specification x



let substitute labels format = 
  let get_label s = 
    try StringMap.find s labels
    with Not_found -> raise (Label_not_defined s)
  in
    match format with 
      | RFormat x -> RFormat.to_char_list x 
      | IFormat t ->
	  let open IFormat in
	  let imm = 
	    match t.immediate with 
	      | Const x -> Const x
	      | Label s -> Const (Int (get_label s))
	  in
	    to_char_list { t with immediate = imm }
      | JFormat t -> 
	  let open JFormat in
	  let ad = 
	    match t.address with
	      | Const x -> Const x
	      | Label s -> Const (get_label s)
	  in
	    to_char_list { t with address = ad }
	      

let make_all program = 
  let text_ctx = List.fold_left text_instruction (new text_context) (fst program) in
  let data_ctx = new data_context ( text_ctx # end_address ) in
  let text_result = text_ctx # result in
  let data_result = ( List.fold_left data_instruction data_ctx (snd program) ) # result in
  let map_merge k x y = 
    match x, y with
      | Some z, None | None, Some z -> Some z
      | None, None -> None 
      | Some _, Some _ -> raise (Label_error k)
  in
  let labels = StringMap.merge map_merge (fst text_result) (fst data_result) in
  let program = List.( concat & map (substitute labels) & snd text_result, snd data_result ) in
    program
