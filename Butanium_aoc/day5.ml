let lines = Read.get_puzzle_lines 5
type point = { mutable x : int; mutable y : int}
type segment = {start : point;  fin : point}
let lower_bound = {x=1000;y=1000}
let upper_bound = {x=0;y=0}
let compute data = Scanf.sscanf data "%d,%d -> %d,%d" (fun x y z t -> 
      upper_bound.x <- max upper_bound.x @@ max x z;
      upper_bound.y <- max upper_bound.y @@ max y t;
      lower_bound.x <- min lower_bound.x @@ min x z;
      lower_bound.y <- min lower_bound.y @@ min y t;
      {start={x;y};fin={x=z;y=t}})
let segments = List.map compute lines 
let part1_segments = List.filter (fun s -> s.start.x = s.fin.x || s.start.y = s.fin.y) segments
let ($) point segment = let lbx, ubx, lby, uby = 
  min segment.start.x segment.fin.x, 
  max segment.start.x segment.fin.x, 
  min segment.start.y segment.fin.y, 
  max segment.start.y segment.fin.y
  in
  point.x >= lbx && point.x <= ubx && 
  point.y >= lby && point.y <= uby && 
  (point.x - segment.start.x)*(segment.fin.y - segment.start.y) = 
  (point.y - segment.start.y)*(segment.fin.x - segment.start.x)
let solve segments part =  
  let rec aux p acc = function 
    | [] -> false 
    | s :: t -> 
      p $ s && acc || aux p (acc || p $ s) t
  in
  let r = ref 0 in 
  for x=lower_bound.x to upper_bound.x do 
    for y=lower_bound.y to upper_bound.y do 
      if aux {x;y} false segments then incr r;
    done
  done;
  Printf.printf "result for part %d : %d\n\n" part !r

let () = solve part1_segments 1; solve segments 2;
print_endline "\n";