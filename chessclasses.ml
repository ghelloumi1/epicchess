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


 (* Format : [(Piece, (liste des déplacements autorisés, (droit de multiplier par k le mouvement (c'est à dire de se déplacer de plus d'une case), droit de suvoler une pièce) *)

 let mouvements = 
    [(Knight, ([(1, 2); (-1, 2); (1, -2); (-1, -2); (2, 1); (2, -1); (-2, 1); (-2, -1)], (false, true))); 
     (Rook, ([(0, 1); (0, -1); (1, 0); (-1, 0)], (true, false)));
     (Bishop, ([(1, 1); (-1, 1); (1, -1); (-1, -1)], (true, false)));
     (King, ([(0, 1); (0, -1); (1, 0); (-1, 0); (1, 1); (-1, 1); (1, -1); (-1, -1)], (false, false)));
     (Queen, ([(0, 1); (0, -1); (1, 0); (-1, 0); (1, 1); (-1, 1); (1, -1); (-1, -1)], (true, false)));
     (Pawn, ([(0, 2); (0,1); (1,1); (-1,1)], (false, false)))
    ]
  ;;



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

   method  king color = if color = White then king_w else king_b
   method castling color = if color = White then castling_w else castling_b
     
   method moves = moves

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

   method mouvements_p (a, b) p castling  =
     (* On récupère les mouvements de la piece *)
    let _, (l, (mult, _)) = List.find (fun (x, y) -> x = p) mouvements in
      (* Si c'est une piece noire, les mouvements sont inversés *)
    let l = if turn = Black then List.map (fun (x, y) -> (-x, -y)) l else l in

      match l, mult with
	| l, false ->  
	    let nl =  List.map (fun (x, y) -> ((a+x, b+y), Queen)) l in
	      (* On rajoute la promotion du cavalier *)
	    let nl' = List.filter (fun (_, y) -> p = Pawn && ((b+y = 7 && turn = White) || (b+y = 0 && turn = Black))) l in
	    let nl' = List.map (fun (x, y) -> ((a+x, b+y), Knight)) nl' in 

	      (* On rajoute le roque  *)
	    let nl'' = 
	      if castling && (p = King) && ((b = 0 && turn = White) || (b = 7 && turn = Black)) then  
		[((a-2, b), Queen); ((a+2, b), Queen)] 
	      else  [] in

	      (* On rajoute la prise en passant *)
	    let nl''' = 
	      if p = Pawn && ((a = 4 && turn = White) || (a = 3 && turn = Black)) then
		[((a-1, b+1), Queen); ((a+1, b+1), Queen)]
	      else [] in
	      List.filter (fun ((x, y), _) -> board#in_bounds (x, y)) (nl'''@nl''@nl'@nl)
	| l, true -> 
	    let rec list i = 
	      if i < 8 then 
		let l = List.map (fun (x, y) -> ((x*i+a, y*i+b), Queen)) l in
		let nl = List.filter (fun ((x, y), _) ->  board#in_bounds (x, y)) l in
		  nl@(list (i+1))
	      else []
	    in
	      list 1

   (* Private methods *)
   method private edit_king color pos = if color = White then king_w <- pos else king_b <- pos
   method private add_move m = moves <- m::moves 
   method private edit_castling color king forward =
     let f = if forward then (+) else (-) in
     let r = match self#castling color with 
	 (r, s) -> if king then (r, f s 1) else (not forward, s) in

       if color = White then castling_w <- r 
       else castling_b <- r

 end




let game = new chess;;
game#mouvements_p (4, 1) Pawn true ;;
game#init;;
game#print;;
game#move_piece (Castling((4,0), (6,0)));;
game#castling Black;;
game#castling White;;
game#king White;;
game#cancel;;
game#edit_castling White;;
game#castling Black;;
