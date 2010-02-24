open Xboard
open Chess

let play game prof = 
  let s, c = alphabeta game prof in
    game#move_piece c;
    s

let xboard () = 
  let game = new chess  in
    game#init;
    let xboard = new xboard in
      xboard#init;
      let rec think () = 
	let _, c = alphabeta game 3 in
	  game#move_piece c;
	  xboard#play c;
	   interact()
      and interact () = 
	let str = read_line () in
	  (match Str.split (Str.regexp " ") str with
	     | ["xboard"] -> Printf.printf "feature myname=\"EpicChess\" done=1\n"; xboard#flush; interact()
	     | ["quit"] ->  raise Exit
	     | s -> 
		 (match xboard#parse_move game (List.hd s) with 
		    | Some c -> 
			game#move_piece c;
			think();
		    | None -> 
			interact()
		 )
	  )
      in
	interact()
;;
let debug () = 
  let prof = ref 3 in
  let scan_move s = Scanf.sscanf s "%d,%d:%d,%d" (fun a b c d-> ((a,b), (c, d))) in
  let scan_prof s = Scanf.sscanf s "p:%d" (fun p -> p) in
  let rec loop game = 
    let s = read_line() in
      try
        let r = scan_prof s in prof := r; loop game
      with _ ->
        (try
           let a, f = scan_move s in
           let r, mvt = game#check_move a f Queen true in
             if not r then (print_endline "Invalid mouvement"; loop game)
             else
	       (
		 game#move_piece (get_option mvt);
                 game#print;
                 let s = play game !prof in 
                   game#print;
                   print_endline (ExtendN.string_of_score s); 
                   loop game
	       )
		 
	 with _ -> print_endline "Invalid command"; loop game)
  in
  let game = new chess in
    game#init;
    game#print;
    let s = play game !prof in
      print_endline (ExtendN.string_of_score s); 
      game#print; ignore (loop game)
;;
let _ = 
  let d = ref "0" in
  let arguments = ["-d", Arg.Set_string d, "1 or 0 : debug mode or not"] in
  Arg.parse arguments (fun _ -> ()) "Usage: see manual.";
    if (!d) = "0" then xboard() else debug()
