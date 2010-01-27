open Printf
exception Invalid_deplacement;;
exception Invalid;;
type piece_type = King | Queen | Rook | Bishop | Knight | Pawn
type color = Black | White
type piece = piece_type * color
type field = Piece of piece | Empty
type dep = 
    Roque of (int * int) * (int * int)
  | Enpassant of (int * int) * (int * int)
  | Prom of (int * int) * (int * int) * piece_type
  | Dep of (int * int) * (int * int)
;;
let (|>) f g = fun x -> f (g x);;

module Aux = 
struct
  let color = function
    | Piece (p, c) -> c
    | Empty -> raise Invalid

  let piece = function
    | Piece (p, c) -> p
    | Empty -> raise Invalid


  let get_option = function
    | Some x -> x
    | None -> raise Invalid 

  let copy_matrix m =
    let r = Array.make_matrix 8 8 Empty in
      for i = 0 to 7 do
	r.(i) <- Array.copy m.(i)
      done;
      r
end

let piece_chars = [(King, 'K'); (Queen, 'Q'); (Rook, 'R'); (Bishop, 'B'); (Knight, 'N'); (Pawn, 'P')]

let char_of_piece_type pt = List.assoc pt piece_chars

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


let initialise_board () = 
  let ch = Array.make_matrix 8 8 Empty in
    for i = 0 to 7 do ch.(i).(1) <- Piece (Pawn, White) done;
    for i = 0 to 7 do ch.(i).(6) <- Piece (Pawn, Black) done;
  let inline_piece = [|Rook; Knight; Bishop; Queen; King; Bishop; Knight; Rook|] in
    for i = 0 to 7 do ch.(i).(0) <- Piece (inline_piece.(i), White) done;
    for i = 0 to 7 do ch.(i).(7) <- Piece (inline_piece.(i), Black) done;
    ch;;


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

let rec is_empty_path ch (a, b) (a', b') (x, y) dist i = 
  if i = dist then true
  else
    if ch.(i*x+a).(i*y+b) <> Empty then false
    else
      is_empty_path  ch (a, b) (a', b') (x, y) dist (i+1)

;;
let is_valid_mouvement ch ycolor (a,b) (a', b') p_prom = 
  let piece_dep = ch.(a).(b) in
    (* la piece a bouger et sa destination sont dans l'echequier *)
    if not (in_board a b) && (in_board a' b') then (false, None)
      (* la piece a bouger existe et est de la couleur du joueur *)
    else if (ch.(a).(b) = Empty || Aux.color ch.(a).(b) <> ycolor ) then (false, None)
      (* la destination de la piece est une case vide ou une piece adverse *)
    else if (try Aux.color ch.(a').(b') = ycolor with _ -> false) then (false, None)
    else
      match piece_dep with
	| Piece(Pawn, col) -> 
	    (* Si il y a une promotion du pion *)
	    let prom = b'= (if col = White then 7 else 0) in 
	      (*Si il n'y a pas de piece adverse *)
	      if ch.(a').(b') = Empty then 
		if a' <> a then (false, None)
		  (* Si on est sur la ligne 1 on peut aller soit a la ligne  2 soit à la ligne 3 *)
		else if 
		  (col = White && b = 1 && b' = 3 && ch.(a).(b+1) = Empty)
		  || (col = Black && b = 6 && b' = 4 && ch.(a).(b-1) = Empty)
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
	    let _, (l, (dep, survol)) = List.find (fun x -> fst x = p) mouvements in
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
		if is_empty_path ch (a, b) (a', b') mvt k 1 then (true, Some (Dep((a, b), (a', b'))))
		else (false, None)
	| _ -> (false, None)

;;
(*
  Roque of (int * int) * (int * int)
  | Enpassant of (int * int) * (int * int)
  | Prom of (int * int) * (int * int) * piece_type
  | Dep of (int * int) * (int * int)
*)
let move_piece board mvt = 
  let nb = Aux.copy_matrix board in 
    match mvt with 
      | Dep ((a, b), (a', b')) -> 
	  nb.(a').(b') <- nb.(a).(b); nb.(a).(b) <- Empty; nb
      | Prom ((a, b), (a', b'), p) ->
	  nb.(a').(b') <- Piece(p, Aux.color nb.(a).(b)); nb.(a).(b) <- Empty; nb
      | _ -> raise Invalid
;;


let rec make_list e n = 
  if n <= 0 then [] else (Aux.copy_matrix e)::(make_list e (n-1));;

let (!!) = function
  | White -> Black
  | Black -> White
;;

let is_there_a_king ch ycolor = 
  let r = ref false in
  for i = 0 to 7 do
    for j = 0 to 7 do
      let p = ch.(i).(j) in
	if p <> Empty && Aux.color p = ycolor && Aux.piece p = King then r := true
    done
  done;
    !r
;;


let get_all_of_a_piece (a, b) p ycolor =
  let _, (l, (mult, _)) = List.find (fun (x, y) -> x = p) mouvements in
  let l = if ycolor = Black then List.map (fun (x, y) -> (-x, -y)) l else l in
    match l, mult with
      | l, false ->  
	  let nl =  List.map (fun (x, y) -> ((a+x, b+y), Queen)) l in
	    (* On rajoute la promotion du cavalier *)
	    (* On regarde quelles sont les pieces suceptible d'aller a la promotion, puis on leur assigne le statut de chevalier *)
	  let nl' = 
	    List.filter (fun (_, y) -> p = Pawn && 
	    ((b+y = 7 && ycolor = White) 
	     || (b+y = 0 && ycolor = Black))) l in
	    
	  let nl' = List.map (fun (x, y) -> ((a+x, b+y), Knight)) nl' in 
	    List.filter (fun ((x, y), _) -> in_board x y) (nl@nl')
      | l, true -> 
	  let rec list i = 
	      if i < 8 then 
		let l = List.map (fun (x, y) -> ((x*i+a, y*i+b), Queen)) l in
                let nl = List.filter (fun ((x, y), _) -> in_board x y) l in
		  nl@(list (i+1))
	      else []
	  in
	    list 1
;;
let rec get_all ch ycolor verif_echec = 
  let list = ref [] in
  for i = 0 to 7 do
    for j = 0 to 7 do
      let pp = ch.(i).(j) in
	if (pp <> Empty) && (Aux.color pp = ycolor) then
	( 
	  let l = get_all_of_a_piece (i, j) (Aux.piece pp) ycolor in
	  let nl = List.map (fun (x, prom) -> (if verif_echec then valid_mouvement else is_valid_mouvement) ch ycolor (i, j) x prom) l in
	  let nl = List.filter (fun (r, x) -> r) nl in 
	    list := List.map (fun (_, dep) -> ( Aux.get_option dep)) nl @ !list
	) done
  done;
    !list
and is_in_echec ch ycolor = 
  let pos_ennemies =  get_all ch (!!ycolor) false in
  let l = List.combine (make_list ch (List.length pos_ennemies)) pos_ennemies in
  let nl = List.map (fun (bd, mvt) -> 
		       is_there_a_king (move_piece bd mvt) ycolor) l in
    not (List.fold_left (&&) true nl)
and valid_mouvement ch ycolor (a, b) (a', b') p_prom = 
  let r, mvt = is_valid_mouvement ch ycolor (a, b) (a', b') p_prom in
  if r then
    let m = Aux.copy_matrix ch in 
      if is_in_echec (move_piece m (Aux.get_option mvt)) ycolor then (false, None)
      else (true, mvt)
  else (false, None)
;; 

(* Regarde si un joueur est en mat *)
let rec is_check_mat board ycolor = 
  (* Si on ne peut faire aucun coup valide alors il y a checkmat*)
   get_all board ycolor true = []
;;
  
let rec points = [(Pawn, 15); (Knight, 45); (Bishop, 45); (Rook, 150); (Queen, 300); (King, 0)];;

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
let eval board ycolor = 
  eval_color board ycolor - eval_color board !!ycolor;;

let rec alphabeta board ycolor alpha beta prof =
    let rec loop max_s al bt = function
    | [] -> Aux.get_option max_s
    | (b, mvt, eval_c)::tail ->
	  if is_check_mat b (!!ycolor) then (10000000, mvt)
	  else
	    let score = 
	      if prof = 0 then eval_c
              else 
		let s, _ = alphabeta b (!!ycolor) (-bt) (-al) (prof-1) in -s
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
  let pos =  get_all board ycolor true in
  let l = List.combine (make_list board (List.length pos)) pos in
  let nl = List.map (fun (board, mvt) -> let b =  move_piece board mvt in (b, mvt, eval b ycolor)) l in
  let l' = List.sort (fun (_, _, a) (_, _, b) -> compare b a) nl in
    loop None alpha beta l'

;;

let play board ycolor prof = 
  let _, mvt = alphabeta board ycolor (-1000000) 1000000 prof in
    move_piece board mvt
;;

let b = Array.make_matrix 8 8 Empty;;
(*
print_board b;;
b.(1).(0) <- Piece(King, White);;
let b = play b White 2;;
let b = play b Black 2;;
let b = initialise_board();;

let l = get_all b White true;;
List.length l;;
*)
let _ = 
  let prof = ref 3 in
  let scan_move s = Scanf.sscanf s "%d,%d:%d,%d" (fun a b c d-> ((a,b), (c, d))) in
  let scan_prof s = Scanf.sscanf s "p:%d" (fun p -> p) in
  let ycolor = Black in
  let rec loop board = 
    let s = read_line() in
      try
	let r = scan_prof s in prof := r; loop board
      with _ ->
    (try
	let a, f = scan_move s in
	let r, mvt = valid_mouvement board ycolor a f Queen in
		   if not r then (print_endline "Invalid mouvement"; loop board)
		   else
		       let b = move_piece board (Aux.get_option mvt) in
		       let b' = play b (!!ycolor) !prof in 
			 print_board b';  loop b'
		    
    with _ -> print_endline "Invalid command"; loop board)
  in
  let board = initialise_board () in
  let board = play board White !prof in
    print_board board;
    loop board
;;
