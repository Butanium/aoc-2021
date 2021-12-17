let lines = ref @@ Read.get_puzzle_lines 9
let dim = List.length !lines 
let take() = match !lines with | [] -> raise Not_found | x :: xs -> lines := xs; Base.String.to_array x
let height_map = Array.init dim (fun _ -> Array.map (fun x -> int_of_string @@ Char.escaped x) @@ take())
type point = {x : int; y : int; height : int}
let ybound = dim - 1
let xbound = Array.length height_map.(0) - 1
let bound b coord =  0 <= coord && coord <= b
let bound (x,y) = bound xbound x && bound ybound y
let (++) = Util.couple_op (+)
let get_height (x,y) = height_map.(y).(x)
let neighbours = [0,1;0,-1;1,0;-1,0]
let get_neighbours p = List.filter bound @@ List.map (fun x -> x ++ p) neighbours

let low_points = Base.Array.foldi height_map ~init:[] ~f:(fun y acc arr -> 
      Base.Array.foldi arr ~init:acc ~f:(fun x acc2 height -> assert (height = get_height (x,y));
          if List.for_all (fun p -> get_height p > height) @@ get_neighbours (x,y) then 
            {x;y;height} :: acc2 else acc2
      )
    )

let () =  Printf.printf "Result for day 9 part 1 : %d\n\n" @@ List.fold_left (fun acc x -> acc + 1 + x.height) 0 low_points


let get_basin low_point = 
  let marked = Array.make_matrix (ybound+1) (xbound+1) false in 
  let to_decode = Queue.create () in 
  Queue.add (low_point.x, low_point.y) to_decode;
  marked.(low_point.y).(low_point.x) <- true; 
  let rec aux acc = if Queue.is_empty to_decode then acc else (
    let point = Queue.take to_decode in
    aux @@ List.fold_left (fun acc ((x,y) as p) ->
        let height = get_height p in 
        if not marked.(y).(x) && height < 9 then (
          Queue.add p to_decode;
          marked.(y).(x) <- true;
          acc + 1
        )  else acc
      ) 0 (get_neighbours point) + acc;
  ) in aux 1

let () =  
  let a,b,c = match List.sort (fun x y -> compare y x) @@ List.map get_basin low_points with
  | a :: b :: c :: _ -> a,b,c  
  | _ -> failwith "failed" in 
  Printf.printf "result for day 9 part 2 : %d | sizes : %d, %d, %d\n" a b c (a*b*c)