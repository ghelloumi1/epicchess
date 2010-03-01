(* open Chess *)
(* open Aux *)

class xboard =
object (self)
  method private castling_line game = 
    if game#turn = White then 0 else 7
  method private promotion_line game = 
    if game#turn = White then 6 else 1
  method private last_line game = 
    if game#turn = White then 7 else 0

  method init =  Sys.set_signal Sys.sigint Sys.Signal_ignore
  method parse_move (game:chess) str = 
    try
    Some (match str with
        | "O-O"   -> Castling((4, self#castling_line game), (6, self#castling_line game)) 
	| "O-O-O" -> Castling((4, self#castling_line game), (2, self#castling_line game)) 
	| s -> 
	   let a, b, a', b' = int_of_letter s.[0], int_of_char s.[1],  int_of_letter s.[2],  int_of_char s.[3] in
            (match String.length s with
            | 4 ->
		let r, d = game#check_move (a, b) (a', b') Queen true in
		  get_option d
            | 5 when (is_digit s.[1] && is_digit s.[3]) ->
		let r, d = game#check_move (a, b) (a', b') (piece_type_of_char s.[4]) true in
		  if r then get_option d else raise Exit
            | _ -> raise Exit)
	 )
    with _ -> None
  method private show_move= function
    | Dep((a, b), (a', b')) 
    | Enpassant((a, b), (a', b')) -> Printf.sprintf "%c%d%c%d"  (letter_of_int a) (b+1) (letter_of_int a') (b'+1)
    | Castling((a, b), (a', b')) when a' < a -> "O-O-O"
    | Castling(_, _) -> "O-O"
    | Prom((a, b), (a', b'), p) ->
	Printf.sprintf "%c%d%c%d%c" (letter_of_int a) (b+1) 
	                     (letter_of_int a') (b'+1) (char_of_piece_type p)
  method play m = 
    print_endline ("move "^(self#show_move m));  self#flush

  method flush = Pervasives.flush Pervasives.stdout

end
