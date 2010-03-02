open Chess
let piece_chars = [(King, 'K'); (Queen, 'Q'); (Rook, 'R'); (Bishop, 'B'); (Knight, 'N'); (Pawn, 'P')]
let rec rassoc x lst =
    match lst with
    | [] -> raise Not_found
    | (a, b) :: t -> if x = b then a else rassoc x t

let piece_type_of_char c = rassoc (Char.uppercase c) piece_chars
let char_of_piece_type pt = List.assoc pt piece_chars

let int_of_char c = int_of_char c - int_of_char '0' - 1
let int_of_letter x = 
    let x = Char.lowercase x in
    if 'a' <= x && x <= 'h' 
        then int_of_char x - int_of_char 'a'
        else raise Not_found
let letter_of_int x = "abcdefgh".[x]

let is_digit c = '0' <= c && c <= '9'

let castling_line game = 
    if game#turn = White then 0 else 7
let promotion_line game = 
    if game#turn = White then 6 else 1
let last_line game = 
    if game#turn = White then 7 else 0


