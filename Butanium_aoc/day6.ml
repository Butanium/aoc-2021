let lines = Read.get_puzzle_lines 6 
let fish_list = List.flatten @@ (List.map @@ (fun l -> List.map int_of_string @@ String.split_on_char ',' l)) lines

let update fish = 
  let rec aux = function
    | [] -> []
    | f :: fs -> if f = 0 then 6::8::aux fs else (f-1) :: aux fs
  in aux fish 
  
let () = let rec aux lim fish = function 
          | i when i = lim -> List.length fish 
          | i -> aux lim (update fish) (i+1)
      in Printf.printf "result for day 6 part 1 : %d " @@ aux 80 fish_list 0

let fish_count1 = Array.make 9 0
let fish_count2 = Array.copy fish_count1
let () = List.iter (fun x -> fish_count1.(x) <- fish_count1.(x) + 1) fish_list
let update flip_flop = 
    let read, write = if flip_flop then fish_count1, fish_count2 else fish_count2, fish_count1 in 
    write.(8) <- read.(0);
    write.(6) <- read.(7) + read.(0);
    write.(7) <- read.(8);
    for i = 0 to 5 do write.(i) <- read.(i+1) done

let () = let rec aux lim flip_flop = function 
    | i when i = lim -> Array.fold_left (+) 0 @@ if flip_flop then fish_count1 else fish_count2
    | i -> update flip_flop; aux lim (not flip_flop) (i+1)
  in Printf.printf "result for day 6 part 2 : %d" @@ aux 256 true 0