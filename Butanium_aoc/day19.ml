(* DISCLAIMER : this works but is not optimized at all (took 20 minutes to run for part 1), I should have used the distances between beacons 
to speed up my pairing policy *)

type beacon = {pos:pos; scanner: scanner} and pos = {coord1 : int; coord2 : int;coord3:int}
and scanner = { id : int;mutable beacons: beacon list; mutable s_pos : scanner_pos}
and scanner_pos = Unknown | Position of pos

let start_time = Sys.time ()

let get_time () = Sys.time () -. start_time
let origin = {coord1=0; coord2=0; coord3=0}
let cartesian_product = Base.List.cartesian_product
let get_pos = [(fun p -> p.coord1); (fun p -> p.coord2); (fun p -> p.coord3)]
let get_pos = get_pos @ List.map (fun f -> fun x -> - f x) get_pos
let permute_coords_funs = 
  let doubles = cartesian_product get_pos get_pos in 
  let triples = cartesian_product get_pos doubles in 
  let test_pos = {coord1=1; coord2=2; coord3=3} in
  let test f = let p = f test_pos in if p.coord1 <> p.coord2 && p.coord1 <> p.coord3  && p.coord2 <> p.coord3 then
    Some f else None in 
  List.filter_map (fun (a,(b,c)) -> 
    let f p = {coord1=a p; coord2=b p; coord3=c p} in test f) triples

let (++) p p' = {coord1=p.coord1+p'.coord1; coord2=p.coord2+p'.coord2; coord3=p.coord3+p'.coord3}

let add_relative_fun = List.map (fun permutation -> fun p1 p2 -> p1 ++ permutation p2) permute_coords_funs

let get_s_pos s = match s.s_pos with | Position p -> p | _ -> failwith "can't get pos of Unknown"
let opp p = {coord1 = -p.coord1; coord2 = -p.coord2; coord3 = -p.coord3}
let match_sonnars b1 b2 (+~) = 
  let new_pos = get_s_pos b1.scanner ++ b1.pos +~ opp b2.pos in 
  assert (new_pos +~ b2.pos = get_s_pos b1.scanner ++ b1.pos);
  b2.scanner.s_pos <- Position new_pos

let count_match couples (+~) =
  Base.List.count ~f:(fun (b1,b2) -> 
    get_s_pos b1.scanner ++ b1.pos = get_s_pos b2.scanner +~ b2.pos) couples 

let change_beacons (+~) scanner =
  scanner.beacons <- List.map (fun b -> {pos = origin +~ b.pos; scanner}) scanner.beacons 


let try_match (+~) scanner  matched_scanner  = 
  let potential_matchs = cartesian_product matched_scanner.beacons scanner.beacons in
  List.exists (fun (b1,b2) -> match_sonnars b1 b2 (+~); 
    let t = count_match potential_matchs (+~) >= 12 in 
    if t then (change_beacons (+~) b2.scanner; Printf.printf "matched %d with %d at t = %.0f\n%!" b2.scanner.id b1.scanner.id (get_time ())); t) potential_matchs

let try_all_matched scanner matched_list (+~) = 
  List.exists (try_match (+~) scanner) matched_list

let try_all_op scanner matched_list = 
  List.exists (try_all_matched scanner matched_list) add_relative_fun


let match_all scanners = 
  let rec aux_match matched_list to_match_list can't_match_list = 
    match to_match_list with 
    | [] -> begin match can't_match_list with [] -> matched_list | l ->  aux_match matched_list l [] end
    | scanner :: tl -> if try_all_op scanner matched_list then aux_match (scanner :: matched_list) tl can't_match_list
    else aux_match matched_list tl (scanner :: can't_match_list)
  in 
  match scanners with 
  | sc :: scs -> let r  = aux_match [sc] scs [] in assert(List.length r = List.length scanners); r
  | _ -> assert false

let parse_scanner scanner_str = 
  let lines = Util.split_on_newline scanner_str in 
  let id = Scanf.sscanf (List.hd lines) "%s scanner %d %s" (fun _ d _ -> d) in
  let scanner = {beacons=[]; s_pos= if id = 0 then Position origin else Unknown; id} in 
  scanner.beacons <- List.map (fun s -> Scanf.sscanf s "%d,%d,%d" (fun coord1 coord2 coord3 -> 
    let pos = {coord1; coord2; coord3} in {pos; scanner})) (List.tl lines);
  scanner

(* let puzzle = Read.get_test () *)
let puzzle =  Read.get_puzzle 19
let scanners_strings = Util.split_parts puzzle

let scanners = List.map parse_scanner scanners_strings

let matched_scanners () = match_all scanners

let debug_scanners_pos l = List.iter (fun s -> 
  let pos = get_s_pos s in 
  Printf.printf "scanner %d at x=%d, y=%d, z=%d\n" s.id pos.coord1 pos.coord2 pos.coord3) 
  @@ List.sort (fun s1 s2 -> compare s1.id s2.id) l


let absolute_pos b = b.pos ++ get_s_pos b.scanner  
let count_distinct = 
  let tbl = Hashtbl.create 1000 in 
  let fold_beacon acc b = if Hashtbl.mem tbl @@ absolute_pos b then acc else (
    Hashtbl.add tbl (absolute_pos b) true;
    acc+1
  )
  in
  List.fold_left (fun acc s -> List.fold_left fold_beacon acc s.beacons) 0

(* let part1 = Printf.printf "result for day 19 part 1 : %d, ran in %g seconds\n" (count_distinct @@ matched_scanners()) *)

let part2 = (* using the debuged information from part 1 because it takes too long run *)
  let positions = Read.get_puzzle_lines 191 in 
  let parse_pos s = Scanf.sscanf s "x=%d, y=%d, z=%d" (fun x y z -> (x,y,z)) in
  let points = List.map parse_pos positions in
  let couples = cartesian_product points points in 
  let result = List.map (fun ((x,y,z),(a,b,c)) -> abs(x-a)+ abs(y-b)+ abs(z-c)) couples in 
  Printf.printf "result for day 19 part 2 : %d\n" @@ List.fold_left max 0 result
