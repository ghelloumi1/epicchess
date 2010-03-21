open Aux
open Chess
open Opens
open ExtendN

let rec take n = function
  | [] -> []
  | a::b when n > 0 -> a::(take (n-1)) b
  | _ -> []
;;

let compare_score a b = if a >> b then 1 else if a >>= b then 0 else -1;; 

let rec insert e = function
  | [] -> [e]
  | a::l -> if fst e >>= fst a then e::a::l else a::insert e l
;;
  let rec alphabeta game alpha beta el prof =
      let rec loop lmoves al bt = function
	| [] -> 
		lmoves
	| (s, mvt)::tail ->
		game#move_piece mvt;
		let score = 
		  if prof <= 0 || s = MInf || s = PInf then s
		  else 
		    let  s, _ = List.hd (alphabeta game ((--) bt) ((--) al) false (prof-1)) in (--) s
		in
		  game#cancel;
		  let best = try List.hd lmoves with _ -> (MInf, Dep((0,0), (0,0))) in
		  let n_best = if score >>= fst (best) then (score, mvt) else best in
		  let n_alpha = if fst n_best >>= al then fst n_best else al in
		      if n_alpha >> bt then [n_best]
		      else
			loop (insert (score, mvt) lmoves) n_alpha bt tail
      in
	(* On récupère et on trie les coups possibles *)
      let nl = 
	(* On trie les coups sur une profondeur de 3 *)
	if el then 
	  let l' = alphabeta game alpha beta false 3 in
	    l'
	else
	  let l = game#get_moves false in
	  let l' = List.map (fun mvt -> 
			   game#move_piece mvt; 
			   let s = game#eval !!(game#turn) in game#cancel; (s, mvt)
			) l in
	     List.sort (fun (a, _) (b, _) -> compare b a) l'
      in
      let r = loop [] alpha beta nl in r
   ;;


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
let rec l_to_tree = List.map (fun x -> Leaf x);;

let rec insert e = function
  | [] -> [e]
  | a::l -> 
      if get_score_of_tree e >>= get_score_of_tree a then e::a::l else a::insert e l
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
             else loop n_best n_alpha bt (insert tree l) tail
   in
      let l = game#get_moves false in
      let nl = List.map (fun mvt -> 
                           game#move_piece mvt; 
                           let s = game#eval !!(game#turn) in game#cancel; (s, mvt)
                        ) l in
      let l' = List.sort (fun (a, _) (b, _) -> compare b a) nl in
      let r, s = (loop MInf alpha beta [] l') in
        (r, s)
;;

let rec prolonge_tree game alpha beta n tree = 
  let rec loop best al bt l = function
    | [] ->  
	(l, best)
    | t::tail ->
	let nt = prolonge_tree game ((--)bt) ((--)al) (n-1) t in
	let s = get_score_of_tree nt in
	let n_best = if s >>= best then s else best in
	let n_alpha = if n_best >>= al then n_best else al in
	  if n_alpha >> bt then  (nt::l@tail, n_best)
	  else loop (if s >>= best then s else best) n_alpha bt (insert nt l) tail
  in
  match tree with
      | Node((s, d), l) ->
          game#move_piece d;
          let r, score = loop MInf alpha beta [] l in
            game#cancel;
	   Node(((--)score, d), r)
      | Leaf(s, d) ->
          game#move_piece d;
          let r, s = alphabeta game alpha beta n in
            game#cancel;
              Node(((--) s, d), r)
      | B l ->
          let nl, _ = loop MInf alpha beta [] l in
            B nl
;;

let rec play g n nb t = 
    if n > nb then t
    else
      play g (n+1) nb (prolonge_tree g MInf PInf n t)
  ;;
let rec search g n = 
  play g 1 n (B (fst (alphabeta g MInf PInf 0)))
;;

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
    snd  (List.hd (alphabeta game MInf PInf true n))
end
;;

