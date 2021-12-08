let positions =  Array.of_list @@ List.map int_of_string @@ String.split_on_char ',' @@ Read.get_puzzle 7
let mediane = Array.sort compare positions; let i = Array.length positions in positions.(i/2) 
let () = Printf.printf "\n\nresult for day 7 part 1 : %d" @@ Array.fold_left (fun acc x -> acc + abs (x-mediane)) 0 positions

let average =  Array.fold_left (+) 0 positions / Array.length positions 
let () =  Printf.printf "\n\nresult for day 7 part 2 : %d, av : %d" (Array.fold_left (fun acc x -> acc + let d =  abs (x-average) in (d * (d+1))/2) 0 positions)
  average