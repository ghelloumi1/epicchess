open Aux
open Chess
 (* open Opens *)
open ExtendN

exception Invalid_tree;;
type feuille = score * dep;;
type tree = 
  | B of tree list
  | Node of feuille * tree list
  | Leaf of feuille
;;

let string_of_tuple (a, b) = 
  Printf.sprintf "(%d, %d)" a b
;;
let print_dep = function
  | Dep(d, a) ->
      Printf.printf "Dep(%s, %s)" (string_of_tuple d)  (string_of_tuple a)
  | Prom(d, a, p) ->
      Printf.printf "Prom(%s, %s, %c)" (string_of_tuple d)  (string_of_tuple a) (char_of_piece_type p)
  | Castling(d, a) ->
      Printf.printf "Cast(%s, %s)" (string_of_tuple d)  (string_of_tuple a)
  | Enpassant(d, a) ->
       Printf.printf "Enpassant(%s, %s)" (string_of_tuple d)  (string_of_tuple a)
;;
let print_leaf (s, d) = 
  print_dep d; print_string (" : " ^ (string_of_score s));;

let print_tree tree = 
  let indent n =
    for x = 1 to n do print_string "|    " done in
  let rec aux tree n = match tree with
  | Leaf a -> indent n; print_leaf a; print_newline ()
  | Node(f, l) -> 
      indent n; 
      print_leaf f; print_newline();
      List.iter (fun t -> aux t (n+1)) l
  | B l ->
      List.iter (fun t -> aux t (n+1)) l

  in aux tree 0
;;

let get_score_of_tree = function
  | Leaf (s, d) -> s
  | Node((s, d), _) -> s
  | _ -> raise Invalid_tree
;; 
let compare_score a b = if a >> b then 1 else if a >>= b then 0 else -1;;  

let rec l_to_tree = List.map (fun x -> Leaf x);;

let reduce f l = 
  List.fold_left f (List.hd l) (List.tl l)
;;

let rec take n = function
  | [] -> []
  | a::b when n > 0 -> a::(take (n-1)) b
  | _ -> []
;;

let rec alphabeta game alpha beta prof =
   let rec loop best al bt l = function
     | [] -> (l, best)
     | (s, mvt)::tail ->
	  game#move_piece mvt; 
	 let tree, s = 
	   if prof <= 0 then (Leaf(s, mvt), s)
	   else 
	     let r, s = alphabeta game ((--) bt) ((--) al) (prof -1) in
	     let s = (--) s in
	     (Node ((s, mvt), r),  s)
	 in
	   game#cancel;
	   let n_best = if s >> best then s else best in
	   let n_alpha = if n_best >>= al then n_best else al in
	     (* 
		Si il y a une coupure, on renvoit le coup qui a produit la coupure, 
		suivit des autres que l'on a déjà calculé ou que l'on ne calcule pas.
		Il seront necessaire pour prolonger l'arbre
	     *)
	     if n_alpha >> bt then (tree::l@(l_to_tree tail), n_best)
	     else loop n_best n_alpha bt (tree::l) tail
   in
      let l = game#get_moves true in
      let nl = List.map (fun mvt -> 
			   game#move_piece mvt; 
			   let s = game#eval !!(game#turn) in game#cancel; (s, mvt)
			) l in
      let l' = take 2 (List.sort (fun (a, _) (b, _) -> compare b a) nl) in
      let r, s = (loop MInf alpha beta [] l') in
      let nl = List.sort (fun a b -> compare_score (get_score_of_tree b) (get_score_of_tree a)) (List.rev r) in
	(nl, s)

;;



let get_score l =  
  let m = reduce (fun (t1, x) (t2, y) -> if x >>= y then (t1, x) else (t2, y)) l in
    snd m
;;

let rec prolonge_tree game alpha beta n tree = 
  let rec loop f best al bt l  = function
  | [] ->
      (l, best)
  | t::tail ->
      let tree, s = prolonge_tree game ((--) bt) ((--) al) (n-1) t in
      let s = f s in
	  print_endline (string_of_score s);
      let n_best = if s >>= best then s else best in
      let n_alpha = if n_best >>= al then n_best else al in
	if n_alpha >> bt then (print_string "coupure"; ([tree](*::l@tail*), n_best)) 
	else loop f n_best n_alpha bt (tree::l) tail
  in
   ( match tree with 
      | Node((s, d), l) ->
	  game#move_piece d;
	  let l, n_score = loop  (--) MInf alpha beta [] l in
	    game#cancel;
	    (* On regarde le nouveau maximum de la branche *)
	      (Node((n_score, d), l), n_score)
      | Leaf(s, d) ->
	  game#move_piece d;
	  let r, s = alphabeta game alpha beta n in
	    game#cancel;
	    let n_score = (--) s in
	      (Node((n_score, d), r), n_score)
      | B l ->
	  let l, s = loop (fun x -> x) MInf alpha beta [] l in
	    (B l, s)
   )
;;

 let g = new chess;;
 g#init;;
 let l, s = alphabeta g MInf PInf 2;;
 
print_tree (B l);;
let t, r = (prolonge_tree g MInf PInf 4 (B l));; 
 print_tree t;;

let t = List.nth l 1;;
 print_tree t;;

let tr, s = prolonge_tree g MInf PInf 2 t;;
 print_tree tr;;
 let tr', s' = prolonge_tree g 5 tr;;
 print_tree tr';;

class ia = 
object (self)
  val game = new chess
  val chess_opening = new opening
  val mutable is_opening = true
  val mutable color = White
  method init c =
    color <- c;
    game#init;
    try chess_opening#fill_book "book.pgn" c 
    with _ -> is_opening <- false

  method game = game
  method move_piece mvt = 
    if is_opening && (game#turn <> color) then 
      (try chess_opening#select_move game mvt 
      with _ -> is_opening <- false);
    game#move_piece mvt

  method think n =
    match is_opening with
      | false ->
	  self#alphabeta n
      | true ->
	  match chess_opening#get_move game with
	    | Some e -> e
	    | None -> is_opening <- false; self#think n
  method alphabeta n = 
    let rec alphabeta game alpha beta ck prof =
      let rec loop best al bt = function
	| [] -> 
	    let sb, cb = best in
	      (* Si on a perdu dans tous les cas *)
	      if sb = MInf then 
		let lm = game#get_moves true in
		  if lm <> [] then (MInf, List.hd lm)
		  else
		    if game#is_check (game#king (game#turn)) then best
		    else
		      (* Si il y a pat *)
		      (N 0, cb)
	      else
		best
	| (s, mvt)::tail ->
		game#move_piece mvt;
		let score = 
		  if prof = 0 || s = MInf || s = PInf then s
		  else let  s, _ = alphabeta game ((--) bt) ((--) al) false (prof-1) in (--) s
		in
		  game#cancel;
		    let n_best = if score >>= (fst best) then (score, mvt) else best in
		    let n_alpha = if fst n_best >>= al then fst n_best else al in
		      if n_alpha >> bt then n_best
		      else
			loop n_best n_alpha bt tail
      in
	(* On récupère et on trie les coups possibles *)
      let l = game#get_moves ck in
      let nl = List.map (fun mvt -> 
			   game#move_piece mvt; 
			   let s = game#eval !!(game#turn) in game#cancel; (s, mvt)
			) l in
      let l' = List.sort (fun (a, _) (b, _) -> compare b a) nl in
	loop (MInf, Dep((0,0), (0,0))) alpha beta l'
    in snd (alphabeta game MInf PInf true n)
end
;;

