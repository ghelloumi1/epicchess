open Chess

class xboard =
object (self)
  val piece_chars = [(King, 'K'); (Queen, 'Q'); (Rook, 'R'); (Bishop, 'B'); (Knight, 'N'); (Pawn, 'P')]
  method private rassoc x lst =
    match lst with
    | [] -> raise Not_found
    | (a, b) :: t -> if x = b then a else self#rassoc x t

  method private piece_type_of_char c = self#rassoc (Char.uppercase c) piece_chars
  method private char_of_piece_type pt = List.assoc pt piece_chars

  method private int_of_char c = int_of_char c - int_of_char '0' - 1
  method private int_of_letter x = 
    let x = Char.lowercase x in
    if 'a' <= x && x <= 'h' 
        then int_of_char x - int_of_char 'a'
        else raise Not_found
  method private letter_of_int x = "abcdefgh".[x]

  method private is_digit c = '0' <= c && c <= '9'

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
	   let a, b, a', b' = self#int_of_letter s.[0], self#int_of_char s.[1],  self#int_of_letter s.[2],  self#int_of_char s.[3] in
            (match String.length s with
            | 4 ->
		let r, d = game#check_move (a, b) (a', b') Queen true in
		  get_option d
            | 5 when (self#is_digit s.[1] && self#is_digit s.[3]) ->
		let r, d = game#check_move (a, b) (a', b') (self#piece_type_of_char s.[4]) true in
		  if r then get_option d else raise Exit
            | _ -> raise Exit)
	 )
    with _ -> None
  method private show_move= function
    | Dep((a, b), (a', b')) 
    | Enpassant((a, b), (a', b')) -> Printf.sprintf "%c%d%c%d"  (self#letter_of_int a) (b+1) (self#letter_of_int a') (b'+1)
    | Castling((a, b), (a', b')) when a' < a -> "O-O-O"
    | Castling(_, _) -> "O-O"
    | Prom((a, b), (a', b'), p) ->
	Printf.sprintf "%c%d%c%d%c" (self#letter_of_int a) (b+1) 
	                     (self#letter_of_int a') (b'+1) (self#char_of_piece_type p)
  method play m = 
    print_endline ("move "^(self#show_move m));  self#flush

  method flush = Pervasives.flush Pervasives.stdout

end
