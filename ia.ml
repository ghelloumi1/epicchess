open Aux
open Chess
open Opens
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
let rec l_to_tree = List.map (fun x -> Leaf x);;

let rec insert e = function
  | [] -> [e]
  | a::l -> 
      if get_score_of_tree e >>= get_score_of_tree a then e::a::l else a::insert e l
;;

let rec alphabeta game alpha beta prof =
   let rec loop best al bt l = function
     | [] ->
        if best = MInf then
	  let lm = game#get_moves true in 
	    if lm <> [] then ([Leaf(MInf, (List.hd lm))], MInf)
	    else 
             if game#is_check (game#king (game#turn)) then ([], MInf)
             else
               (* Si il y a pat *)
               ([], N 0)
	else
	  (l, best)
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
             else loop n_best n_alpha bt (if s >> MInf then insert tree l else l) tail
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

let rec play game s nb tree = 
    if s > nb then tree
    else
      play game (s+1) nb (prolonge_tree game MInf PInf s tree)
  ;;
let get_fist = function
  | B (Node((_, d), _)::_) -> d
  | _ -> raise Invalid_tree
;;
let get_move = function
  | Node((_, d), l) -> d
  | Leaf(_, d) -> d
  | _ -> raise Invalid_tree
;;

(* 
   Select a move played in the tree
   move = None if ia is playing, else move = Some (oposment move)
*)
let select_move move = function
  | B l -> 
      (match (if move = None then List.hd else List.find (fun x -> get_move x = get_option move)) l with
	| Node((_, d), moves) -> B moves
	| _ -> raise Invalid_tree
      )
  | _ -> raise Invalid_tree
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
    let t = B (fst (alphabeta game MInf PInf 0)) in
    let tree = play game 1 n t in
    get_fist tree
end
