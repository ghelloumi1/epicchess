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

(* #use "board.ml";; *)


class chess = 
object (self)
  val mutable board = new board (8, 8) Empty
  val mutable turn = White
  val mutable king_w = (4,0)
  val mutable king_b = (4,6)
  val mutable castling_w = (true, 0)
  val mutable castling_b = (true, 0)

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
  
  method castling color = if color = White then castling_w else castling_b
  method edit_castling color king forward =
    let f = if forward then (+) else (-) in
    let r = match self#castling color with 
	(r, s) -> if king then (r, f s 1) else (not forward, s) in

      if color = White then castling_w <- r 
      else castling_b <- r

    
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
	      self#edit_castling turn true true
	    end
      |  Prom ((a, b), (a', b'), p) ->
	   board#set_point (a', b') (Piece(p, turn));
	   board#delete (a, b)
      | Castling ((a, b), (a', b')) ->
	  let ooo = a' < a in
	    (if ooo then 
		board#move (0,b) (3,b)
	    else
		board#move (7,b) (5,b));
            board#move (a, b) (a', b);
	    self#edit_castling turn false true;
	    self#edit_king turn (a', b')
      | Enpassant ((a, b), (a', b')) ->
	  board#move (a, b) (a', b');
	  board#delete (a', b)
      | _ -> raise Invalid
    end;
    self#edit_turn;
    self#add_move mvt;
    board#end_move

  method cancel = 
    board#rollback;
    self#edit_turn;
    match List.hd moves with
      | Castling((a, b), (a', b')) ->
	  self#edit_castling turn false false;
	  self#edit_king turn (a, b)
      | Dep ((a, b), (a', b')) when get_piece (board#get_point (a', b')) = King ->
	  self#edit_castling turn true false;
	  self#edit_king turn (a, b)
      | _ -> ()


  method copy = 
    let g = new chess in
      g#fill board turn king_w king_b castling_w castling_b moves;
      g

end



let game = new chess;;
game#init;;
game#print;;
game#move_piece (Castling((4,0), (6,0)));;
game#castling Black;;
game#castling White;;
game#king White;;
game#cancel;;
game#edit_castling White;;
game#castling Black;;
