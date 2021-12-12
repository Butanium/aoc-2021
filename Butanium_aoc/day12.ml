let lines = Read.get_puzzle_lines 12
let graph = Hashtbl.create 20
let add_link s1 s2 = 
  let f s sa= Hashtbl.replace graph s @@ sa :: match Hashtbl.find_opt graph s with | None -> [] | Some l -> l
  in f s1 s2; f s2 s1 
let extract_node s = Scanf.sscanf s "%[^-]-%s" (fun s1 s2 -> add_link s1 s2)
let fill_hashtable = List.iter extract_node lines
let debug () = Hashtbl.iter (fun s l -> Printf.printf "\n\n%s -> " s; List.iter (Printf.printf "%s, ") l) graph
let is_big s = String.capitalize_ascii s = s
let get_or_default table key default = match Hashtbl.find_opt table key with Some x -> x | None -> default
let is_possible visits is_part2 = function 
  | "start" -> false 
  | s -> is_big s || get_or_default visits s 0 <= if is_part2 then Hashtbl.find visits "have time" else 0

let copy_visits visits node = 
  let p = get_or_default visits node 0 in 
  let visits_copy = Hashtbl.copy visits in 
  Hashtbl.replace visits_copy node (p+1);
  if p = 1 && not @@ is_big node then Hashtbl.replace visits_copy "have time" 0;
  visits_copy
let get_paths is_part2 = 
  let rec aux cur_path acc_paths visits cur_node = 
      List.fold_left (fun acc node -> 
        if node = "end" then (List.rev @@ node :: cur_path) :: acc else 
          if is_possible visits is_part2 node then aux (node::cur_path) acc (copy_visits visits node) node else
            acc
      ) acc_paths @@ Hashtbl.find graph cur_node
  in 
  let visits = Hashtbl.create 20 in 
  Hashtbl.replace visits "have time" @@ if is_part2 then 1 else 0;
  aux ["start"] [] visits "start"

let () = Printf.printf "\nresult for day 12 part 1 : %d\n\n" (List.length @@ get_paths false);
         Printf.printf  "result for day 12 part 2 : %d"
         (List.length @@ get_paths true)
  
