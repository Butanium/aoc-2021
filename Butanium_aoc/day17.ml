let puzzle = Read.get_puzzle 17
(* let puzzle = Read.get_test () *)
type bound = {low : int ; high : int}
let xBound, yBound = Scanf.sscanf puzzle "target area: x=%d..%d, y=%d..%d" 
  (fun x1 x2 y1 y2 -> {low=x1;high=x2}, {low=y1; high=y2})

let part1 = Printf.printf "day 17 part 1 : %d\n" (let speed = -yBound.low - 1 in speed*(speed+1)/2)

let in_bound_x x = xBound.low <= x && x <= xBound.high
let in_bound_y y = yBound.low <= y && y <= yBound.high
let in_bound (x,y) =  in_bound_x x && in_bound_y y
let check speedx speedy = 
  let rec aux (x,y) speedx speedy =
    in_bound (x,y) || ((speedx >= 0 || in_bound_x x) && y >= yBound.low && x <= xBound.high && 
    aux (x+speedx,y+speedy) (max 0 @@ speedx-1) (speedy-1))
  in aux (0,0) speedx speedy
let part2 = 
  let r = ref 0 in
  for yspeed = yBound.low to -yBound.low - 1 do 
    for xspeed = 0 to xBound.high do 
      if check xspeed yspeed then (
        (* Printf.printf "valid launch param : %d,%d\n" xspeed yspeed; *)
        incr r
      )
    done 
  done;
  Printf.printf "result for day 17 part 2 : %d\n" !r