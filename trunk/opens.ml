exception Invalid_pgn;;
exception Invalid_pgn_format;;
exception No_file

open Chess
type result = Win | Loose | Draw | Unknow;;

class opening = 
object (self)
  val mutable book = []
  val mutable file = None

  method parse_pgn = 
    let rec parse c s in_read = match (input_line c, in_read) with
      | ("", false) -> parse c s true
      | ("", true) -> s
      | (_, false) -> parse c s in_read
      | (line, true) -> parse c ((if s = "" then "" else s^" ")^line) true
    in 
    let tuple_of_list = function
      | a::b::[] -> (a, b)
      | _ -> raise Invalid_pgn
    in
    let parse_game s =
      (* We split by the numbers moves*)
      let l = Str.split (Str.regexp "[0-9]+\\.") s in 
      (* We split by the spaces *)
      let l_moves = List.map (Str.split (Str.regexp "[ ]+")) l in
      let l' = List.rev l_moves in 
	(* We check the result *)
      let r, l_moves = List.hd (List.rev (List.hd l')), List.rev (List.tl l') in
      let moves = List.map tuple_of_list l_moves in
	match r with 
	  | "1-0" -> (Win, moves)
	  | "0-1" -> (Loose, moves)
	  | "1/2-1/2" -> (Draw, moves)
	  | "*" -> (Unknow, moves)
	  | _ -> print_string r; raise Invalid_pgn_format
    in
      match file with
	| Some c -> parse_game (parse c "" false)
	| None -> raise No_file
  method private forward = 
     let rec tail acc = function
      | [] -> acc
      | (r, _::l)::list when l <> [] ->
	  tail ((r, l)::acc) list
      | _::list -> tail acc list
     in
       book <- tail [] book
  method fill_book fbook turn  = 
    try 
      let f = open_in fbook in
	file <- Some f;
	let moves = ref [] in
	let result = if turn = White then Win else Loose in
	  try
	    while true do
	      let r, m = self#parse_pgn  in
		if m <> [] && r = result  then moves := (r, m)::!moves
	    done
	  with End_of_file -> book <- !moves
    with Sys_error _ -> raise No_file

  method select_move move turn = 
    let f = if turn = White then fst else snd in
      book <- List.filter (fun (r, l) -> f(List.hd l) = move) book;
      if turn = Black then self#forward

  method get_move turn = 
    let rec max_l me list = match me, list with
      | me, [] -> me
      | Some(c, nm), (s, n)::l -> max_l (Some (if n > nm then (s, n) else (c, nm))) l
      | None, (s, n)::l -> max_l (Some (s, n)) l
    in
    let rec max_moves f l = function
      | [] -> max_l None l
      | (r, moves)::tail ->
	  let c = f (List.hd moves) in
	    if List.mem_assoc c l then
	      max_moves f ((c, (List.assoc c l)+1)::(List.remove_assoc c l) ) tail
	    else
	      max_moves f ((c, 1)::l) tail
    in

    let f = if turn = White then fst else snd in 
    let r = max_moves f [] book in
      begin match r with
	| Some(m, _) ->
	   book <- List.filter (fun (r, l) -> f (List.hd l) = m) book; 
	| None -> ()
      end;
      if turn = Black then self#forward;
      r
  method get_b = book
end
;;
