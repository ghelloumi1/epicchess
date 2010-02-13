open Array
open Printf 

exception Fill_InvalidSize;;
exception OutOfBounds;;

type position = int * int

type 'a accessor = 
    Linear of position * (int * int) (* Linear line, with the last tuple being the direction vector *)
  | Interval of ('a -> bool) *  position * position * (int * int)
  | Horizontal of position
  | Vertical of position

let (++) (a,b) (c,d) = (a+c) , (b+d)

let copy_matrix m (sx,sy) def =
  let r = make_matrix sx sy def in
    for i = 0 to sx-1 do
      r.(i) <- copy m.(i)
    done;
    r

let rec take n l = match (n,l) with
  | (_, []) -> []
  | (0, _) -> []
  | (n, x::xs) -> x::(take (n-1) xs)

let rec repeat e = function
  | 0 -> []
  | n -> e::(repeat e (n-1))

let rec repeat_num n = function
  | c when c = n -> []
  | c -> c::(repeat_num n (c+1))
 let int_al = [(0, ('0', fun _ -> ' '));(1, ('1', fun _ -> ' '));
	       (2, ('2', fun _ -> '*'));(3, ('3', fun _ -> ' '));
	       (4, ('4', fun _ -> '*'));(5, ('5', fun _ -> ' '));
	       (6, ('6', fun _ -> '*'));(7, ('7', fun _ -> ' '));
	       (8, ('8', fun _ -> '*'));(9, ('9', fun _ -> ' '))] (*Default representation for ints - adds a start next to pair numbers*)


 let test = [| [| 1; 4; 7|];
	       [| 2; 5; 8|];
	       [| 3; 6; 9|];
	       [| 1; 2; 3|] |]

 class ['a] board (sx, sy) empty = 
   object (self)
     val mutable board = make_matrix sx sy (empty : 'a)

     method copy = 
       let n = new board (sx,sy) empty
       in n#fill board; n 

     method fill nb = 
       let p  = length nb = sx
       and p' = snd (fold_left (fun (s, valid) e -> if (length e = s && length e = sy) && (valid = true)
				then (s, true) else (s, false)) 
		       (length nb.(0),true) nb)
       in if p && p' then board <- (copy_matrix nb (sx,sy) empty) else (raise Fill_InvalidSize)

 (* The association list is of type (key, (char_value, pred)). The predicates return a char added after the char_value (used to differenciate colors, for ex *)
     method print al = (* Takes an association list to know how to represent various types *)
       let separator = repeat "+----+" sx in
       print_string "\n   "; List.map print_string separator; print_string "\n";
       for j = sy-1 downto 0 do
	 printf " %d |" (j);
	 for i = 0 to sx-1 do
	   match board.(i).(j) with
	     | e when e = empty -> print_string "    |"
	     | e -> let (char_val, pred) = List.assoc board.(i).(j) al
	       in printf " %c%c  |" (pred e) char_val 
	done;
	print_string "\n   "; List.map print_string separator; print_string "\n";
      done;
      let repeat_nums = repeat_num sx 0
      and repeat_space = repeat "     " sx
      in print_string "\n  "; List.iter2 (fun x y -> print_string x; print_int y) repeat_space repeat_nums; print_string "\n"

    method set_point p e = self#raw_set p e
    method get_point p = self#raw_get p
    method get = function 
	  | Horizontal p -> self#get (Linear (p, (0,1)))
	  | Vertical p   -> self#get (Linear (p, (1,0)))
	  | Interval (p, s, e, u) -> List.filter p (self#interval s e u)
	  | Linear (p, c) -> self#linear p c

(* Private methods *)
	      
    method private interval s e u = 
      let rec interval' = function
	| c when c <> e && self#in_bounds c -> (self#raw_get c)::(interval' (c++u))
	| c when self#in_bounds c -> [self#raw_get c]
	| _ -> raise OutOfBounds
      in interval' s
    method private linear (x,y) (a,b) = 
      let reduced = match (a,b) with
	| (_, 0) -> (0, y)
	| (0, _) -> (x, 0)
	| (_, _) -> let m = min x y in (x-m, y-m)
      in let rec linear' = function 
	| (x', y') when self#in_bounds (x',y') -> (self#raw_get (x', y'))::(linear' (x'+a, y'+b))
	| _ -> []
      in linear' reduced
    method private in_bounds (x,y) = (x >= 0) && (x <= sx) && (y >= 0) && (y <= sy)
    method private raw_get (x,y) = board.(x).(y)
    method private raw_set (x,y) e = board.(x).(y) <- e
  end;;
