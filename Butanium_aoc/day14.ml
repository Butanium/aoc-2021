(* let puzzle = Read.get_test() *)
let puzzle = Read.get_puzzle 14
let chars, instructions = match Util.split_parts puzzle with | [x;y] -> x,y | _ -> failwith "parse error"

let chars = Base.String.to_list chars

let instr_table = Hashtbl.create 10
let fill_table = List.iter (fun s -> Scanf.sscanf s "%c%c -> %c" (fun start fin to_insert -> 
    Hashtbl.add instr_table (start, fin) to_insert)) 
   @@ Util.split_on_newline instructions

let update chars = 
  let rec aux acc = function
    | [] -> []
    | c :: cs -> match Hashtbl.find_opt instr_table (acc, c) with 
                  | None -> c :: aux c cs 
                  | Some ins -> ins :: c :: aux c cs
  in 
  aux '1' chars

let rec loop lim i acc_l = 
  if i = lim then acc_l 
  else loop lim (i+1) (update acc_l)

let part1_chars = loop 10 0 chars

let get_minmax_count chars = 
  let count_table = Hashtbl.create 10 in 
  List.iter (fun x -> Hashtbl.replace count_table x @@ Util.get_or_default count_table x 0 + 1 ) chars;
  Hashtbl.fold (fun _ v (min_c, max_c) -> min min_c v, max max_c v) count_table (max_int, 0)

let min_count, max_count = get_minmax_count part1_chars
  

let () = Printf.printf "result for day 14 part 1 : %d, max : %d, min : %d\n\n" (max_count - min_count) max_count min_count

let couple_table = Hashtbl.create 100
let start_char = List.hd chars
let end_char = List.fold_left (fun _ x -> x) '1' chars 

let add_start_couple = 
  let rec aux last = function
    | [] -> ()
    | x :: xs -> Util.incr_hashtable couple_table (last,x); aux x xs
  in 
  aux start_char @@ List.tl chars;
  Printf.printf "finished adding couples\n%!"

let update ()= 
  let all_instr = ref @@ fun () -> () in
  let add_unit unit_f = let old_f = !all_instr in all_instr :=
     fun () -> (unit_f (); old_f ()) in
  Hashtbl.iter (fun ((s,e) as k) amount -> Printf.printf "updatating the %d : %c,%c couple\n%!" amount s e;
      match Hashtbl.find_opt instr_table k with 
      | Some replace -> let add = Util.add_hashtable amount couple_table  in
        add_unit (fun () -> add (s, replace));
        add_unit (fun () -> add (replace, e));
        add_unit (fun () -> Util.add_hashtable (-amount) couple_table k)
      | None -> ()
    ) couple_table;
  Printf.printf "finished update, runnning unit : \n%!";
  !all_instr ();
  Printf.printf "finished unit \n%!"

let get_minmax_count () = 
  let count_table = Hashtbl.create 10 in 
  Hashtbl.iter (fun (s,e) amount -> let add = Util.add_hashtable amount count_table in 
                add s; add e) couple_table;
  Util.incr_hashtable count_table end_char; (* add 1 because they are counted only once on the edges *)
  Util.incr_hashtable count_table start_char;
  Hashtbl.fold (fun _ v (min_c, max_c) -> min min_c v, max max_c v) count_table (max_int, 0)



let debug () = Hashtbl.iter (fun (s,e) v -> Printf.printf "couple : %c,%c, amount : %d\n" s e v) couple_table

let perform_part2 = for _ = 1 to 40 do update () done

let min_count, max_count = get_minmax_count ()
let () = Printf.printf "result for day 14 part 2 : %d, max : %d, min : %d" ((max_count - min_count)/2) max_count min_count