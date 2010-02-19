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
type score = PInf | N of int | MInf;;
let (--) = function
  | PInf -> MInf
  | MInf -> PInf
  | N n -> N (-n)
;;
let (>>) a b = match (a, b) with
  | PInf, MInf
  | MInf, PInf -> PInf
  | PInf, PInf | MInf, MInf -> a
  | MInf, N n 
  | N n, MInf -> N n
  | PInf, N n 
  | N n, PInf -> PInf
  | N n, N m -> N (max n m)
;;
type dep = 
    Castling of (int * int) * (int * int)
  | Enpassant of (int * int) * (int * int)
  | Prom of (int * int) * (int * int) * piece_type
  | Dep of (int * int) * (int * int)
;;

let get_piece = function
    | Piece(p, c) -> p
    | Empty -> print_string "get_piece"; raise Invalid
let get_color = function
  | Piece(p, c) -> c
  | Empty -> raise Invalid
;;
let get_option = function
  | Some e -> e
  | None -> raise Invalid
;;
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
   val mutable king_b = (4,7)
   val mutable castling_w = (true, 0)
   val mutable castling_b = (true, 0)

   val mutable moves = []
   
   method apply b = board#fill b
   method init = 
     let b = Array.make_matrix 8 8 Empty in
       for i = 0 to 7 do b.(i).(1) <- Piece (Pawn, White) done;
       for i = 0 to 7 do b.(i).(6) <- Piece (Pawn, Black) done;
       let inline_piece = [|Rook; Knight; Bishop; Queen; King; Bishop; Knight; Rook|] in
	 for i = 0 to 7 do b.(i).(0) <- Piece (inline_piece.(i), White) done;
	 for i = 0 to 7 do b.(i).(7) <- Piece (inline_piece.(i), Black) done;
	 board#fill b;
	 turn <- White

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
	   (f Knight, ('N', stared));
	   (f Pawn,   ('P', stared));
	  ]
     in
       print_endline (if turn = White then "White's turn" else "Black's turn");
       board#print piece_al



   method turn = turn
   method edit_turn = turn <- !!turn

   method  king color = if color = White then king_w else king_b
   method castling color = if color = White then castling_w else castling_b
     
   method moves = moves

   method edit_king color pos = if color = White then king_w <- pos else king_b <- pos
   method private add_move m = moves <- m::moves 
   method private edit_castling color king forward =
     let f = if forward then (+) else (-) in
     let r = match self#castling color with 
	 (r, s) -> if king then (r, f s 1) else (not forward, s) in

       if color = White then castling_w <- r 
       else castling_b <- r

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
     try
	 moves <- List.tl moves
       with _ -> ();

     try
       match List.hd moves with
	 | Castling((a, b), (a', b')) ->
	     self#edit_castling turn false false;
	     self#edit_king turn (a, b)
	 | Dep ((a, b), (a', b')) when get_piece (board#get_point (a', b')) = King  ->
	     self#edit_castling turn true false;
	     self#edit_king turn (a, b)
	 | _ -> () 
     with _ -> ()

   method  mouvements_p (a, b) p castling  =
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
   method private check_enpassant (a,b) (a', b') = 
    if not ((b = 4 && b' = 5 && turn = White) 
	    || (b = 3 && b' = 2 && turn = Black)) then false
    else 
      match moves with
	| Dep((x, y), (x', y'))::l -> 
	    if get_piece (board#get_point (x', y')) = Pawn then 
	      if abs(y'-y) = 2 && abs (a'-a) = 1 then
		if a' = x' then true
		else false
	      else false
	    else false
	| _ -> false
   method private check_castling (a, b) (a', b') = 
     let s, n = self#castling turn in
       if n > 0 || (not s) then false
       else if not (b = b') then false
       else if b = 0 && turn = Black then false
       else if b = 7 && turn = White then false
       else 
	  let k, r = board#get_point(a, b), board#get_point ((if a' < a then 0 else 7), b) in
	    if k = Empty || get_piece k <> King || get_color k <> turn then false
	    else if r = Empty ||  get_piece r <> Rook || get_color r <> turn then false
	    else  
		if not (board#get_interval ((=) Empty) (a, b) ((if a' < a then 0 else 7), b)  ((if a' < a then -1 else 1), 0)) then false
		else 
		  not (self#is_check (a, b) || (if a' < a then 
					     self#is_check (a-1, b) ||
					     self#is_check (a-2, b)
					   else
					     self#is_check (a+1, b) ||
					     self#is_check (a+2, b)
					  )
		      )
   method check_move (a, b) (a', b') p_prom castling = 
    let r, mvt = self#is_check_move(a, b) (a', b') p_prom castling in
      if r then begin
	(* blanc *)
	self#move_piece (get_option mvt);
	self#edit_turn;
	 let r' = self#is_check (self#king turn) in
	self#edit_turn;
	self#cancel;
	  if r' then (false, None) else (true, mvt)
      end
      else (false, None)
		  
	(* Look if you are check *)
   method private is_check pos_king =
     self#edit_turn;
    let mvt_adv = self#get_all false false in
     self#edit_turn;
    let f = function 
      | Dep (_, a) | Prom (_, a, _) -> 
	  a = pos_king
      | _ -> false
    in
    let l = List.map f mvt_adv in 
      (List.fold_left (||) false l) 


   method  is_check_move (a,b) (a', b') p_prom castling = 
     (* la piece a bouger et sa destination sont dans l'echequier *)
     if not (board#in_bounds (a, b) && board#in_bounds (a', b')) then (false, None)
     else
       let piece_dep = board#get_point (a, b) in
       let piece_destination = board#get_point (a', b') in
	 (* la piece a bouger existe et est de la couleur du joueur *)
	 if (piece_dep = Empty || (get_color piece_dep) <> turn ) then (false, None) 
	   (* la destination de la piece est une case vide ou une piece adverse *)
	 else if (try get_color piece_destination = turn with _ -> false) then (false, None)
	 else
	   match piece_dep with
	     | Piece(Pawn, col) -> 
		 (* Si c'est une prise en passant *)
		 if self#check_enpassant (a, b) (a', b') then (true, Some(Enpassant((a, b), (a', b'))))
		 else
		   (* Si il y a une promotion du pion *)
		   let prom = b'= (if col = White then 7 else 0) in 
		     (*Si il n'y a pas de piece adverse *)
		     if piece_destination  = Empty then 
		       if a' <> a then (false, None)
			 (* Si on est sur la ligne 1 on peut aller soit a la ligne  2 soit à la ligne 3 *)
		       else if 
			 (col = White && b = 1 && b' = 3 && board#get_point (a, b+1) = Empty)
			 || (col = Black && b = 6 && b' = 4 && board#get_point (a, b-1) = Empty)
		       then (true, Some (Dep((a, b), (a', b'))))
			 (* Sinon on ne peut avance que de 1*)
		       else if (if col = White then b'-b else b-b') = 1 then
			 if prom then (true, Some (Prom((a, b), (a', b'), p_prom))) else (true, Some (Dep((a, b), (a', b'))))
		       else (false, None)
			 (* Sinon on peut manger une piece sur les cotés *)
		     else if 
		       if col = White then (b'-b = 1) && (a'-a = 1 || a'-a = -1)
		       else  (b-b' = 1) && (a-a' = 1 || a-a' = -1)
		     then 
		       if prom then (true, Some (Prom((a, b), (a', b'), p_prom))) else (true, Some (Dep((a, b), (a', b'))))
		     else (false, None)
	     | Piece(p, c) ->
		 (* Si c'est un roque *)
		  if p = King && castling && self#check_castling (a, b) (a', b') then 
		       (true, Some (Castling((a, b), (a', b'))))
		  else 
		 let _, (l, (dep, flyover)) = List.find (fun x -> fst x = p) mouvements in
		   (* k représente la distance en cases entre la d'arrivée et de départ pour le fou, le roi, la dame et la tour. *)

		 let k =  max(abs(a'-a)) (abs(b'-b)) in
		 let l_pos = 
		   if dep then
		     List.map (fun (x, y) -> ((x, y), (x*k+a, y*k+b))) l
		   else
		     List.map (fun (x, y) -> ((x, y), (x+a, y+b))) l
		 in
		   (* On vérifie si le déplacement est bien dans ceux autorisés, et on regarde si oui quelle direction il utilise. *)
		 let mvt, r = 
		   try let m, a = List.find (fun (a, (x, y)) ->  a' = x && b' = y) l_pos in (m, true)
		   with _ -> ((0,0), false)
		 in
		   (* Vérifie que le chemin est libre, et qu'on ne vole pas au dessus de pieces.*)
		   if flyover then
		     if r then (true, Some (Dep((a, b), (a', b'))))
		     else (false, None)
		   else if board#get_interval ((=) Empty) (a, b) (a', b') mvt then (true, Some (Dep((a, b), (a', b'))))
		   else (false, None)
		      
	     | _ -> (false, None)

   method private printc (a, b) = 
     print_int a; print_string " : "; print_int b; print_newline()
   method private get_all check_king castling  = 
     let list = ref [] in
      for i = 0 to 7 do
	 for j = 0 to 7 do
	   let pp = board#get_point(i, j) in
	     if (pp <> Empty) && (get_color pp = turn) then
	       (
		 self#printc (i, j);
		 let l = self#mouvements_p (i, j) (get_piece pp) castling in
		   print_string "ok";
		   let nl = List.map (fun (c, prom) -> (if check_king then self#check_move else self#is_check_move) (i, j) c prom castling) l in 
		 let nl = List.filter (fun (r, x) -> r = true) nl in  
		   list := List.map (fun (_, dep) -> (get_option dep)) nl @ !list;
	       )
	     done
       done; 
       !list
   method get_moves check_king = 
     self#get_all check_king true

 end



 let b = Array.create_matrix 8 8 Empty;;
 b.(2).(2) <- Piece(King, White);;
 b.(4).(6) <- Piece(King, Black);;
 let g = new chess;;
 g#apply b;;
 g#edit_king White (2,2);;
 g#edit_king Black (4,6);;
 g#print;;
 g#get_moves false;;
 let l = g#moves;;
 List.iter g#move_piece l;;
let play game = 
   let l =  (game#get_moves true) in
   let c = List.nth l (Random.int (List.length l)) in

     game#move_piece c;
     game#print
;;
play g;;
Random.self_init();;
let g = new chess;;
g#init;;
g#print;;
let l = g#moves;;
List.iter g#move_piece l;;
g#mouvements_p (4,7) Queen false;;
play g;;
g#turn;;
