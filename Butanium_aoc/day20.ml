let puzzle = Read.get_puzzle 20
(* let puzzle = Read.get_test () *)
let neigbours = List.rev [-1,-1 ; 0,-1 ; 1,-1; -1,0 ; 0,0 ; 1,0 ; -1,1 ; 0,1 ; 1,1]
let (++) = Util.couple_op (+)
(* let add_neighbours pos add_table default = 
  List.iter (fun p -> let _ = Util.find_or_set_default add_table (pos++p) default in ()) neigbours *)

let get_index pos get_table default = 
  fst @@ List.fold_left (fun (acc, pow) p -> 
    acc + pow * Util.get_or_default get_table (pos++p) default, pow*2) (0,1) neigbours

let dic_str, datas_str = match Util.split_parts puzzle with [x;y] -> x,y | _ -> failwith "parse error"

let to_int = function '#' -> 1 | '.' -> 0 | _ -> failwith "parse error" 
let dictionary = Array.map (to_int) @@ Base.String.to_array dic_str
let rec enhance get_table set_table default count =
  if count = 0 then get_table else begin 
    let new_neighbours = Stack.create () in 
    let add_neighbours pos = 
      List.iter (fun p -> if not @@ Hashtbl.mem get_table (p++pos) then Stack.push (p++pos) new_neighbours) neigbours 
    in
    let update p = Hashtbl.replace set_table p dictionary.(get_index p get_table default) in
    Hashtbl.iter (fun p _ -> 
      add_neighbours p;
      update p
      ) get_table; 
    Stack.iter update new_neighbours;
    let default = if default = 0 then dictionary.(0) else dictionary.(511) in
    Printf.printf "%d enhancing remaining, table size : %d\n%!" (count-1) (Hashtbl.length set_table);
    enhance set_table get_table default (count-1)
  end

let start_enhance times = 
  let start_table = Hashtbl.create 100000 in
  let char_m = Read.lines_to_matrix @@ Array.of_list @@ Util.split_on_newline datas_str in 
  Util.iteri_matrix (fun y x c -> Hashtbl.add start_table (x,y) @@ to_int c) char_m;
  enhance start_table (Hashtbl.create 100000) 0 times



let part1 = let table = start_enhance 2 in 
  let r = Hashtbl.fold (fun _ x acc -> x + acc) table 0 in 
  Printf.printf "result for day 20 part 1 : %d\n%!" r
   
  let part2 = let table = start_enhance 50 in 
    let r = Hashtbl.fold (fun _ x acc -> x + acc) table 0 in 
    Printf.printf "result for day 20 part 2 : %d\n" r
    