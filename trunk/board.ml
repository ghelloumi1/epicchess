open Array

exception Fill_InvalidSize;;

type position = int * int

type accessor = 
    Linear of position * (int * int) (* Linear line, with the last tuple being the direction vector *)
  | Horizontal of position
  | Vertical of position
  | Diagonal of position

let (++) (a,b) (c,d) = a+b , c+d

let copy_matrix m (sx,sy) def =
  let r = Array.make_matrix sx sy def in
    for i = 0 to sx-1 do
      r.(i) <- Array.copy m.(i)
    done;
    r

let test = [| [| 1; 2; 3 |];
	      [| 4; 5; 6 |];
              [| 7; 8; 9 |] |]

class ['a] board (sx, sy) def = 
  object (self)
    val mutable board = make_matrix sx sy (def : 'a)

    method copy = 
      let n = new board (sx,sy) def
      in n#fill board; n 

    method fill nb = 
      let p  = length nb = sx
      and p' = snd (fold_left (fun (s, valid) e -> if (length e = s) && (valid = true)
			       then (s, true) else (s, false)) 
		      (length nb.(0),true) nb)
      in if p && p' then board <- (copy_matrix nb (sx,sy) def) else (raise Fill_InvalidSize)


    method set_point p e = self#raw_set p e
    method get_point p = self#raw_get p
    method get_linear = function 
	  | Horizontal p -> self#get_linear (Linear (p, (0,1)))
	  | Vertical p   -> self#get_linear (Linear (p, (1,0)))
	  | Diagonal p   -> self#get_linear (Linear (p, (1,1)))
	  | Linear (p, c) -> self#linear p c

(* Private methods *)

    method private linear (x,y) (a,b) = 
      let reduced = match (a,b) with
	| (_, 0) -> (0, y)
	| (0, _) -> (x, 0)
	| (_, _) -> let m = min x y in (x-m, y-m)
      in let rec linear' = function 
	| (x', y') when self#in_bounds (x',y') -> (self#raw_get (x', y'))::(linear' (x'+a, y'+b))
	| _ -> []
      in linear' reduced
    method private in_bounds (x,y) = (x >= 0) && (x < sx) && (y >= 0) && (y < sy)
    method private raw_get (x,y) = board.(x).(y)
    method private raw_set (x,y) e = board.(x).(y) <- e
  end;;
