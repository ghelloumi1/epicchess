open Printf
type piece_type = King | Queen | Rook | Bishop | Knight | Pawn
type color = Black | White
type piece = piece_type * color
type field = Piece of piece | Empty

let (!!) = function
  | Black -> White
  | White -> Black
;;
#use "board.ml";;

class chess = 
object (self)
  val mutable board = new board (8, 8) Empty
  val mutable turn = White
  val mutable king_w = (4,0)
  val mutable king_b = (4,6)
  val mutable castling_w = true
  val mutable castling_b = true
  val mutable moves = []

  method init = 
    let b = Array.make_matrix 8 8 Empty in
      for i = 0 to 7 do b.(i).(1) <- Piece (Pawn, White) done;
      for i = 0 to 7 do b.(i).(6) <- Piece (Pawn, Black) done;
      let inline_piece = [|Rook; Knight; Bishop; Queen; King; Bishop; Knight; Rook|] in
	for i = 0 to 7 do b.(i).(0) <- Piece (inline_piece.(i), White) done;
	for i = 0 to 7 do b.(i).(7) <- Piece (inline_piece.(i), Black) done;
	board#fill b
  method edit_board b = board <- b
  method print = 
    let piece_al = 
      let stared = function 
	  Piece (pt, c) -> if c = White then ' ' else '*'  
	| _ -> ' '
      and f piece_type = function
	| Piece (ptype, _) -> piece_type = ptype
	| _ -> false
      in [(f King,   ('K', stared));
	  (f Queen,  ('Q', stared));
	  (f Rook,   ('R', stared));
	  (f Bishop, ('B', stared));
	  (f Knight, ('K', stared));
	  (f Pawn,   ('P', stared));
	 ]
    in
      board#print piece_al
  method turn = turn
  method edit_turn t = turn <- t
  method move s e = board#move s e
  method copy = 
    let g = new chess in
      g#edit_board (board#copy);
      g#edit_turn (turn);
      g

end


let game = new chess;;
game#init;;
game#turn;;
game#move (4,1) (4,3);;

let e = game#copy;;
e#move (4,6) (4,4);;
e#print;;
game#print;;
