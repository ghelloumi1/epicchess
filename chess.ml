open Printf
exception Invalid_deplacement;;
exception Invalid;;
exception Invalid_color;;
type piece_type = King | Queen | Rook | Bishop | Knight | Pawn
type color = Black | White
type piece = piece_type * color
type field = Piece of piece | Empty
type dep = 
    Castling of (int * int) * (int * int)
  | Enpassant of (int * int) * (int * int)
  | Prom of (int * int) * (int * int) * piece_type
  | Dep of (int * int) * (int * int)
;;
type position = 
    {
      turn: color;
      board: field array array;
      king_w:(int * int); king_b:(int * int);
      castling_w:bool; castling_b:bool;
      moves: dep list
    }
;;

let (|>) f g = fun x -> f (g x);;

module Aux = 
struct
  let color = function
    | Piece (p, c) -> c
    | Empty -> raise Invalid_color

  let piece = function
    | Piece (p, c) -> p
    | Empty ->( print_string "Aux.piece"; raise Invalid)


  let get_option = function
    | Some x -> x
    | None -> (print_string "Aux.get_option"; raise Invalid )

  let copy_matrix m =
    let r = Array.make_matrix 8 8 Empty in
      for i = 0 to 7 do
	r.(i) <- Array.copy m.(i)
      done;
      r
end

let (!!) = function
  | White -> Black
  | Black -> White
;;

module Position = 
struct
  let edit_turn p  = 
    { p with turn = !!(p.turn)}
  let edit_board p board = 
    { p with board = board }
  let edit_king p pos color = 
    if color = White then 
      { p with king_w = pos }
    else
      { p with king_b = pos }
  let edit_castling p c color = 
    if color = White then
      { p with  castling_w = c}
    else
      { p with  castling_b = c}
  let get_castling p color = 
    if color = White then p.castling_w else p.castling_b
  let get_king p color = 
    if color = White then p.king_w else p.king_b
  let add_move p m = 
    { p with moves = m::p.moves }
end

module Chess = 
struct
  let piece_chars = [(King, 'K'); (Queen, 'Q'); (Rook, 'R'); (Bishop, 'B'); (Knight, 'N'); (Pawn, 'P')]

  let char_of_piece_type pt = List.assoc pt piece_chars;;

  let print_board ar = 
    let separator = "\n   +----+----+----+----+----+----+----+----+\n" in
      print_string separator;
      for j = 7 downto 0 do
	printf " %d |" (j);
	for i = 0 to 7 do
          match ar.(i).(j) with
            | Piece(pt, c) -> printf " %c%c |" (if c = White then ' ' else '*') (char_of_piece_type pt) 
            | Empty -> print_string "    |"
	done;
	print_string separator;
      done;
      print_string "\n      0    1    2    3    4    5    6    7\n"
  ;;
  let initialise_board () = 
    let ch = Array.make_matrix 8 8 Empty in
      for i = 0 to 7 do ch.(i).(1) <- Piece (Pawn, White) done;
      for i = 0 to 7 do ch.(i).(6) <- Piece (Pawn, Black) done;
      let inline_piece = [|Rook; Knight; Bishop; Queen; King; Bishop; Knight; Rook|] in
	for i = 0 to 7 do ch.(i).(0) <- Piece (inline_piece.(i), White) done;
	for i = 0 to 7 do ch.(i).(7) <- Piece (inline_piece.(i), Black) done;
	ch;;



  let init () = 
    {
      turn = White;
      board = initialise_board();
      king_w = (4,0); king_b = (4,7);
      castling_w = true; castling_b = true;
      moves = []
    }
  ;;

  let in_board a b = (a >= 0 && a < 8) && (b >= 0 && b < 8) ;;
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


  let is_good_path board pred (a, b) (dirx, diry) dist = 
    let rec is_good_path' ch (a, b) (x, y) dist i = 
      if i = dist then true
      else
	if not (pred (ch.(i*x+a).(i*y+b))) then false
	else
	  is_good_path' ch (a, b) (x, y) dist (i+1)
    in
      is_good_path' board (a, b) (dirx, diry) (dist+1) 1
  ;;

end
(*
  Roque of (int * int) * (int * int)
  | Enpassant of (int * int) * (int * int)
  | Prom of (int * int) * (int * int) * piece_type
  | Dep of (int * int) * (int * int)
*)
module Rules = 
struct
  let move_piece pos mvt = 
    let nb = Aux.copy_matrix pos.board in 
    let r = match mvt with 
      | Dep ((a, b), (a', b')) -> 
	  nb.(a').(b') <- nb.(a).(b); 
	  nb.(a).(b) <- Empty; 
	  let n_pos = Position.edit_board pos nb in
	    if (Aux.piece nb.(a').(b')) = King then 
	      Position.edit_castling (Position.edit_king n_pos (a', b') pos.turn) false pos.turn
	    else n_pos
      | Prom ((a, b), (a', b'), p) ->
	  nb.(a').(b') <- Piece(p, pos.turn);
	  nb.(a).(b) <- Empty;
	  Position.edit_board pos nb
      | Castling((a, b), (a', b')) ->
	  let ooo = a' < a in
	    if ooo then 
	      begin
		nb.(3).(b) <-nb.(0).(b);
		nb.(0).(b) <- Empty;
		nb.(a').(b) <- nb.(a).(b);
		nb.(a).(b) <- Empty
	      end
	    else 
	      begin
		nb.(5).(b) <- nb.(7).(b);
		nb.(7).(b) <- Empty;
		nb.(a').(b) <- nb.(a).(b);
		nb.(a).(b) <- Empty
	      end;
	    let n_pos = Position.edit_castling (Position.edit_king pos (a', b') pos.turn) false pos.turn in
	      Position.edit_board n_pos nb
      | Enpassant((a, b), (a', b')) ->
	  nb.(a').(b') <- nb.(a).(b);
	  nb.(a').(b) <- Empty;
	  nb.(a).(b) <- Empty;
	  Position.edit_board pos nb
	    
    in
      Position.edit_turn (Position.add_move r mvt)
  ;;

  (*

    let rec make_list e n = 
    if n <= 0 then [] else e::(make_list e (n-1));;
  *)
  let get_all_of_a_piece (a, b) p ycolor castling  =
    let _, (l, (mult, _)) = List.find (fun (x, y) -> x = p) Chess.mouvements in
    let l = if ycolor = Black then List.map (fun (x, y) -> (-x, -y)) l else l in
      match l, mult with
	| l, false ->  
	    let nl =  List.map (fun (x, y) -> ((a+x, b+y), Queen)) l in
	      (* On rajoute la promotion du cavalier *)
	    let nl' = 
	      List.filter (fun (_, y) -> p = Pawn && 
		  ((b+y = 7 && ycolor = White) 
		   || (b+y = 0 && ycolor = Black))) l in
	      
	    let nl' = List.map (fun (x, y) -> ((a+x, b+y), Knight)) nl' in 
	      (* On rajoute le roque  *)
	    let nl'' = 
	      if castling && (p = King) && ((b = 0 && ycolor = White) || (b = 7 && ycolor = Black)) then 
		[((a-2, b), Queen); ((a+2, b), Queen)] 
	      else  [] in
	      (* On rajoute la prise en passant *)
	    let nl''' = 
	      if p = Pawn && ((a = 4 && ycolor = White) || (a = 3 && ycolor = Black)) then
		[((a-1, b+1), Queen); ((a+1, b+1), Queen)]
	      else [] in
	      List.filter (fun ((x, y), _) -> Chess.in_board x y) (nl'''@nl''@nl'@nl)
	| l, true -> 
	    let rec list i = 
	      if i < 8 then 
		let l = List.map (fun (x, y) -> ((x*i+a, y*i+b), Queen)) l in
		let nl = List.filter (fun ((x, y), _) -> Chess.in_board x y) l in
		  nl@(list (i+1))
	      else []
	    in
	      list 1
  ;;

  let is_valid_enpassant game (a,b) (a', b') = 
    if not ((b = 4 && b' = 5 && game.turn = White) 
	    || (b = 3 && b' = 2 && game.turn = Black)) then false
    else 
      match game.moves with
	| Dep((x, y), (x', y'))::l -> 
	    if Aux.piece (game.board.(x').(y')) = Pawn then 
	      if abs(y'-y) = 2 && abs (a'-a) = 1 then
		if a' = x' then true
		else false
	      else false
	    else false
	| _ -> false
  ;;

  let rec is_valid_mouvement game (a,b) (a', b') p_prom = 
    let piece_dep = game.board.(a).(b) in
      (* la piece a bouger et sa destination sont dans l'echequier *)
      if not (Chess.in_board a b) && (Chess.in_board a' b') then (false, None)
	(* la piece a bouger existe et est de la couleur du joueur *)
      else if (game.board.(a).(b) = Empty || (Aux.color game.board.(a).(b)) <> game.turn ) then (false, None) 
	(* la destination de la piece est une case vide ou une piece adverse *)
      else if (try Aux.color game.board.(a').(b') = game.turn with _ -> false) then (false, None)
      else
	match piece_dep with
	  | Piece(Pawn, col) -> 
	      (* Si c'est une prise en passant *)
	      if is_valid_enpassant game (a, b) (a', b') then (true, Some(Enpassant((a, b), (a', b'))))
	      else
		
		(* Si il y a une promotion du pion *)
		let prom = b'= (if col = White then 7 else 0) in 
		  (*Si il n'y a pas de piece adverse *)
		  if game.board.(a').(b') = Empty then 
		    if a' <> a then (false, None)
		      (* Si on est sur la ligne 1 on peut aller soit a la ligne  2 soit à la ligne 3 *)
		    else if 
		      (col = White && b = 1 && b' = 3 && game.board.(a).(b+1) = Empty)
		      || (col = Black && b = 6 && b' = 4 && game.board.(a).(b-1) = Empty)
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

	      (* Si les blancs roquent à droite *)
	      (*if (a = 0 && b = 4) && p = King &&  a' = 0 && b' = 7 then
		(true, Some[(6,0); *)
	      let _, (l, (dep, survol)) = List.find (fun x -> fst x = p) Chess.mouvements in
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

		if survol then 
		  if r then (true, Some (Dep((a, b), (a', b'))))
		  else (false, None)
		else 
		  if Chess.is_good_path game.board (fun x -> x = Empty) (a, b) mvt (k-1) then (true, Some (Dep((a, b), (a', b'))))
		  else 
		    if p = King then 
		      if is_valid_castling game (a, b) (a', b') then (true, Some (Castling((a, b), (a', b'))))
		      else (false, None)
		    else (false, None)
	  | _ -> (false, None)

  and is_valid_castling game (a, b) (a', b') = 
    if not (Position.get_castling game (game.turn)) then false
    else if not (b = b') then false
    else if b = 0 && game.turn = Black then false
    else if b = 7 && game.turn = White then false
    else
      let k, r = game.board.(a).(b), game.board.(if a' < a then 0 else 7).(b) in
	if k = Empty || Aux.piece k <> King || Aux.color k <> game.turn then false
	else if r = Empty ||  Aux.piece r <> Rook || Aux.color r <> game.turn then false
	else
	  let dist = if a' < a then 3 else 2 in 
	    if not (Chess.is_good_path game.board ((=) Empty) (a, b) ((if a' < a then -1 else 1), 0) dist) then false
	    else
	      not (is_check game ||
		     (if a' < a then 
			is_check (Position.edit_king game (a-1, b) game.turn) ||
			  is_check (Position.edit_king game (a-2, b) game.turn)
		      else
			is_check (Position.edit_king game (a+1, b) game.turn) ||
			  is_check (Position.edit_king game (a+2, b) game.turn)
		     ))
  and get_all pos verif_echec  = 
    let list = ref [] in
      for i = 0 to 7 do
	for j = 0 to 7 do
	  let pp = pos.board.(i).(j) in
	    if (pp <> Empty) && (Aux.color pp = pos.turn) then
	      ( 
		let l = get_all_of_a_piece (i, j) (Aux.piece pp) pos.turn verif_echec in
		let nl = List.map (fun (x, prom) -> (if verif_echec then valid_mouvement else is_valid_mouvement)  
				     pos  (i, j) x prom) l in 
		let nl = List.filter (fun (r, x) -> r = true) nl in  
		  list := List.map (fun (_, dep) -> ( Aux.get_option dep)) nl @ !list
	      ) done
      done;
      !list
  and is_check game =
    let pos_king = Position.get_king game (game.turn) in
    let mvt_adv = get_all (Position.edit_turn game) false in
    let f = function 
      | Dep (_, a) | Prom (_, a, _) -> 
	  a = pos_king
      | _ -> false
    in
    let l = List.map f mvt_adv in 
      (List.fold_left (||) false l)

  and valid_mouvement game (a, b) (a', b') p_prom = 
    let r, mvt = is_valid_mouvement game (a, b) (a', b') p_prom in
      if r then
	if is_check (Position.edit_turn (move_piece game (Aux.get_option mvt))) then  (false, None)
	else (true, mvt)
      else (false, None)
  ;;
end

(* Regarde si un joueur est en mat *)
let rec is_check_mat board = 
  (* Si on ne peut faire aucun coup valide alors il y a checkmat *)
  Rules.get_all board true = []
;;

module Eval = 
struct
  let max_score = 1000000;;
  let rec points = [(Pawn, 15); (Knight, 45); (Bishop, 45); (Rook, 150); (Queen, 300); (King, max_score)];;

  let board_center = [|
    [|  0;  0;  0;  0;  0;  0;  0;  0|];
    [|  0;  0;  0;  0;  0;  0;  0;  0|];
    [|  0;  0;  3;  5;  5;  3;  0;  0|];
    [|  0;  0;  5;  9;  9;  5;  0;  0|];
    [|  0;  0;  5;  9;  9;  5;  0;  0|];
    [|  0;  0;  3;  5;  5;  3;  0;  0|];
    [|  0;  0;  0;  0;  0;  0;  0;  0|];
    [|  0;  0;  0;  0;  0;  0;  0;  0|]
  |]
  ;;

  let eval_color board ycolor = 
    let s = ref 0 in
      for i = 0 to 7 do
	for j = 0 to 7 do
	  let p = try Some board.(i).(j) with _ -> None in
	  let r = 
	    (match p with
		 Some (Piece(p, c)) when c = ycolor -> snd (List.find (fun (a, b) -> a = p) points) + board_center.(i).(j)
	       | _ -> 0
	    ) in
	    
	    s := !s + r 
	done
      done;
      !s
  ;;
  let eval game  = 
    eval_color game.board game.turn - eval_color game.board !!(game.turn);;
end

let rec alphabeta game alpha beta prof =
  let rec loop max_s al bt  = function
    | [] -> Aux.get_option max_s
    | (b, mvt, eval_c)::tail ->
	let score = 
	  if prof = 0 then eval_c
          else 
	    let s, _ = alphabeta b (-bt) (-al) (prof-1) in -s
        in
	let nalpha = max score al in
	  if nalpha > bt then (score, mvt)
	  else
	    loop
	      (match max_s with
		 | None -> Some (score, mvt)
		 | Some(s, d) -> Some (if score > s then (score, mvt) else (s, d))
	      )
	      nalpha bt tail
  in
  let l =  Rules.get_all game false in
  let nl = List.map (fun mvt -> let b = Rules.move_piece game mvt in
		       (b, mvt, Eval.eval (Position.edit_turn b))) l in
  let l' = List.sort (fun (_, _, a) (_, _, b) -> compare b a) nl in
    loop None alpha beta l'

;;


let rec alphabeta' game alpha beta prof =
  let rec loop max_s al bt  = function
    | [] -> Aux.get_option max_s
    | (b, mvt, eval_c)::tail ->
        if is_check_mat b then (Eval.max_score, mvt)
        else
          let score =
            if prof = 0 then eval_c
            else
              let s, _ = alphabeta b (-bt) (-al) (prof-1) in -s
          in
          let nalpha = max score al in
            if nalpha > bt then (score, mvt)
            else
              loop
                (match max_s with
                   | None -> Some (score, mvt)
                   | Some(s, d) -> Some (if score > s then (score, mvt) else (s, d))
                )
                nalpha bt tail
  in
  let l =  Rules.get_all game true in
  let nl = List.map (fun mvt -> let b = Rules.move_piece game mvt in
                       (b, mvt, Eval.eval (Position.edit_turn b))) l in
  let l' = List.sort (fun (_, _, a) (_, _, b) -> compare b a) nl in
    loop None alpha beta l'


;;

let play game prof n = 
  let s, mvt = (if n mod 2 = 0 then alphabeta else alphabeta') game (-Eval.max_score/10) (Eval.max_score/10) prof in
  let sgame = Rules.move_piece game mvt in
    (match sgame.moves with
       | Enpassant(_, _)::l -> print_string "en passant !";
       | _ -> ());
    (s,  sgame)
;;

let _ = 
  let prof = ref 5 in
  let scan_move s = Scanf.sscanf s "%d,%d:%d,%d" (fun a b c d-> ((a,b), (c, d))) in
  let scan_prof s = Scanf.sscanf s "p:%d" (fun p -> p) in
  let rec loop game = 
    let s = read_line() in
      try
	let r = scan_prof s in prof := r; loop game
      with _ ->
	(*   (try *)
	let a, f = scan_move s in
	let r, mvt = Rules.valid_mouvement game a f Queen in
	  if not r then (print_endline "Invalid mouvement"; loop game)
	  else
	    (
	      let game = Rules.move_piece game (Aux.get_option mvt) in  
		Chess.print_board game.board;
		print_string "ok";
		let s, game = play game !prof 2 in 
		  Chess.print_board game.board;
		  print_int s; print_newline();
		  loop game)
	      
  (*   with _ -> print_endline "Invalid command"; loop game) *)
  in
  let game = Chess.init() in
    Chess.print_board game.board;
    let s, game = play game !prof 2 in
      print_int s; print_newline(); 
      Chess.print_board game.board;
      loop game
;;

(*
let _ = 
  let prof = ref 5 in
  let i = ref 0 in
  let b = ref (Chess.init()) in
    while true do
      print_endline (if (!b).turn = White then "Blanc" else "Noir");
      let s, r = play !b !prof !i in
	Chess.print_board r.board; 
	print_int s; print_newline(); 
	b := r;
	i := !i+1;
	prof := if !i mod 2 = 0 then 5 else 3;
    done
;;
*)
