let matrix = Read.get_puzzle_int_matrix 15
(* let matrix = Read.get_test_int_matrix() *)
let ybound = Array.length matrix - 1
let xbound = Array.length matrix.(0) - 1


(* module PointSet = 
  Set.Make(struct 
    type t = int*int 
    let score (x,y) = (matrix.(y).(x) + dist_to_end (x,y), ybound - y, xbound - x)
    let compare p1 p2 = compare (score p1) (score p2)
  end)  *)
exception Finish of int array array
let shortest_path xbound ybound matrix end_point = 
  let dist_to_end (x,y) = xbound + ybound - y - x in

  let get_neighbours = Util.get_neighbours ~allow_diags:false ybound xbound in 
  let cost (x,y) = matrix.(y).(x) in
  let costs = Util.map_matrix (fun _ -> -1) matrix in
  costs.(0).(0) <- 0;
  let path_cost (x,y) = costs.(y).(x) in   
  let is_usefull p = List.exists (fun (x,y) -> costs.(y).(x) = -1) @@ get_neighbours p in

  let rec aux min_to_end (frontiere : (int * int) list) =
    let real_frontiere = List.filter is_usefull frontiere in 
      let fold_neigh acc p  = List.fold_left 
          (fun ((min_to_e, is_end, min_dist, min_points) as acc) neigh -> let tot_cost = path_cost p + cost neigh in 
            if path_cost neigh <> -1 || min_dist < tot_cost then acc else
              if min_dist = tot_cost then (min min_to_e @@ dist_to_end neigh, is_end || neigh = end_point, min_dist, neigh :: min_points) else 
              (min min_to_e @@ dist_to_end neigh,neigh = end_point, tot_cost, [neigh])
          )
         acc @@ get_neighbours p
    in 
    let min_to_e, is_end, min_dist, min_points = List.fold_left fold_neigh (min_to_end,false, max_int, []) real_frontiere in 
    if min_to_e < min_to_end then Printf.printf "new best : %d\n%!" min_to_e;
    let new_frontiere = List.fold_left (fun acc ((x,y) as p) -> costs.(y).(x) <- min_dist; p::acc) frontiere min_points in
    if is_end then min_dist else
    aux min_to_e new_frontiere
  in 
  aux max_int [0,0], costs

    
(* let part1 = let start_time = Sys.time() in 
  let r, costs = shortest_path xbound ybound matrix (xbound, ybound) in
  Printf.printf "result day 15 part 1 : %d, t : %f%!\n"  r
  (Sys.time () -. start_time); r,costs *)

let matrix2 = 
  let m = Array.make_matrix (5*(ybound+1)) (5*(xbound+1)) (-1) in 
  for i = 0 to 4 do 
    for j = 0 to 4 do 
      for x = 0 to xbound do
        for y = 0 to ybound do 
          m.(i*(ybound+1) + y).(j*(xbound+1) + x) <-  let w = matrix.(y).(x) + i + j in (w mod 10) + (w/10);
        done 
      done 
    done 
  done;
  m
let shortest_path_opt xbound ybound matrix end_point = 
  let get_neighbours = Util.get_neighbours ~allow_diags:false xbound ybound in 
  let cost (x,y) = matrix.(y).(x) in
  let path_costs = Util.map_matrix (fun _ -> 10 * (xbound + ybound)) matrix in
  path_costs.(0).(0) <- 0;
  let path_cost (x,y) = path_costs.(y).(x) in   
  let update_cost (x,y) cost = path_costs.(y).(x) <- cost in
  let module Heap = Binary_heap.Make(struct 
    type t = int*int 
    let compare p1 p2  = compare (path_cost p1) (path_cost p2) 
  end) in
  let heap = Heap.create ~dummy:(-1,-1) @@ 2*xbound*ybound in 
  Util.iteri_matrix (fun y x _ -> Heap.add heap (x,y)) path_costs;
  let x,y = Heap.minimum heap in 
  Printf.printf "filled heap, size : %d, first : %d,%d %!\n" (Heap.length heap) x y;


  while not @@ Heap.is_empty heap do 
    let p = Heap.pop_minimum heap in 
    (* Printf.printf "deque : %d,%d which has path cost : %d\n%!" x y @@ path_cost p; *)
    List.iter (fun neighbour -> 
      let tot_cost = path_cost p + cost neighbour in 
      if path_cost neighbour > tot_cost then (
          (* Printf.printf "%d,%d <- %d\n%!" (fst neighbour) (snd neighbour) tot_cost; *)
          update_cost neighbour tot_cost;
          Heap.add heap neighbour)
      ) @@ get_neighbours p
  done;
  path_cost end_point, path_costs


let part1_opt = 
  let start_time = Sys.time() in 
  let r, costs = shortest_path_opt xbound ybound matrix (xbound, ybound) in
  Printf.printf "result day 15 part 1 : %d, t : %f%!\n"  r
  (Sys.time () -. start_time); r,costs

let part2 = let start_time = Sys.time() in
  let xbound2 = 5 * (xbound+1)-1 in  
  let ybound2 = 5* (ybound+1)-1 in 
  let r, costs = shortest_path_opt xbound2 ybound2 matrix2 (xbound2, ybound2) in
  Printf.printf "result day 15 part 2 : %d, t : %f\n" r
  (Sys.time () -. start_time); r,costs 


  