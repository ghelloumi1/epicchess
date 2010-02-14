open Printf
type piece_type = King | Queen | Rook | Bishop | Knight | Pawn
type color = Black | White
type piece = piece_type * color
type field = Piece of piece | Empty
exception Invalid
let (!!) = function
  | Black -> White
  | White -> Black
;;
type dep = 
    Castling of (int * int) * (int * int)
  | Enpassant of (int * int) * (int * int)
  | Prom of (int * int) * (int * int) * piece_type
  | Dep of (int * int) * (int * int)
;;

let get_piece = function
    | Piece(p, c) -> p
    | Empty -> raise Invalid

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

  method fill b t kw kb cw cb (m:dep list) =
    board <- b#copy;
    turn <- t;
    king_w <- kw; king_b <- kb;
    castling_w <- cw; castling_b <- cb;
    moves <- m

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
  method edit_turn = turn <- !!turn

  method king color = if color = White then king_w else king_b
  method edit_king color pos = if color = White then king_w <- pos else king_b <- pos
  
  method castling color = if color = White then castling_w else castling_w
  method edit_castling color = if color = White then castling_w <- false else castling_b <- false
    
  method moves = moves
  method add_move m = moves <- m::moves 

  method move s e = board#move s e

  method move_piece mvt = 
    board#start_move;
    begin match mvt with
      | Dep ((a, b), (a', b')) -> 
	  board#move (a, b) (a', b');
	  if get_piece (board#get_point (a', b')) = King then 
	    begin
	      self#edit_king turn (a', b');
	      self#edit_castling turn 
	    end
      |  Prom ((a, b), (a', b'), p) ->
	   board#set_point (a', b') (Piece(p, turn));
	   board#delete (a, b)
      | Castling ((a, b), (a', b')) ->
	  let ooo = a' < a in
	    if ooo then 
		board#move (0,b) (3,b)
	    else
		board#move (7,b) (5,b);
            board#move (a, b) (a', b);
	    self#edit_castling turn;
	    self#edit_king turn (a', b')
      | Enpassant ((a, b), (a', b')) ->
	  board#move (a, b) (a', b');
	  board#delete (a', b)
      | _ -> raise Invalid
    end;
    self#edit_turn;
    self#add_move mvt;
    board#end_move
  method cancel = b#rollback
  method copy = 
    let g = new chess in
      g#fill board turn king_w king_b castling_w castling_b moves;
      g

end



let game = new chess;;
game#init;;
game#print;;
game#move_piece (Castling ((4,0), (6,0)));;

game#turn;;

let e = game#copy;;
e#move (4,6) (4,4);;
e#edit_turn;;
e#print;;
e#edit_turn
game#print;;
e#turn;;
game#turn;;
