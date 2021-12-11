let squids = Read.get_puzzle_matrix 11
(* let squids = Read.get_test_matrix () *)
let simulate squids = 
  let explosion_queue = Queue.create () in 
  let upgrade_squid is_conflict y x squid  = if squid = 9 then (
    Queue.add (x,y) explosion_queue; 0
  ) else squid + 1 - max 0 (if is_conflict then 1-squid else 0)
  in
  Util.mapi_matrix_in_place (upgrade_squid false) squids;
  let rec conflicts acc =
    if Queue.is_empty explosion_queue then
      acc else (
      List.iter (fun (x,y) -> squids.(y).(x) <- (upgrade_squid true) y x squids.(y).(x)) @@
        Util.get_neigbours (Queue.take explosion_queue) 9 9;
      conflicts (acc + 1)
    )
  in 
  conflicts 0 

let rec iter_sim squids times acc = 
  if times = 0 then acc else iter_sim squids (times - 1) (simulate squids + acc)
let squids_copy = Array.map Array.copy squids

let () = Printf.printf "result for day 11 part 1 : %d\n" @@ iter_sim squids_copy 100 0

let rec get_first_full_flash acc squids = 
  if simulate squids = 100 then acc else get_first_full_flash (acc + 1) squids

let () = Printf.printf "result for day 12 part 2 : %d\n" @@ get_first_full_flash 1 squids