let lines = Read.get_puzzle_lines 2 in 
let instructions = List.map (fun s -> Scanf.sscanf s "%s %d" (fun t d -> t,d)) lines in 

let aux s = match s.[0] with 
  | 'f' -> 1,0
  | 'd' -> 0, 1
  | 'u' -> 0, -1
  | _ -> failwith @@ Printf.sprintf "%s is not a valid instrucion" s
in 
let (++) (a,b) (c,d) = a+c,b+d in 
let ($) (a,b) x = a*x,b*x in 
let horizontal, depth = List.fold_left (fun acc (ins,v) -> acc ++ (aux ins $ v)) (0,0) instructions in 
Printf.printf "result day 2 part 1 : %d, %d horizontal, %d depth\n" (horizontal*depth) horizontal depth;

let aux s = match s.[0] with 
  | 'f' -> 1, 1, 0
  | 'd' -> 0, 0, 1
  | 'u' -> 0, 0, -1
  | _ -> failwith @@ Printf.sprintf "%s is not a valid instrucion" s
in 
let (++-) (a,b,x) (c,d,y) = a+c,b+d,x+y in 
let ($-) (a,b,c) x = a*x,b*x,c*x in 
let ($$-) (a,b,x) (c,d,y) = a*c,b*d,x*y in 
let horizontal, depth,_= List.fold_left (fun ((_, _, aim) as acc) (ins,v) -> acc ++- (aux ins $$- (1, aim, 1) $- v)) (0,0,0) instructions in 
Printf.printf "result day 2 part 2 : %d, %d horizontal, %d depth" (horizontal*depth) horizontal depth
