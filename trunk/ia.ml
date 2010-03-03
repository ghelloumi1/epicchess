open Chess
open Opens
open ExtendN
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
