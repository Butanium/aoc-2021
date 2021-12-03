let readlines file = let ic = open_in file in 
let rec aux() = 
  try let s = input_line ic in s :: aux() with  
  End_of_file -> []
in aux()

let get_puzzle_lines day = 
  readlines @@ Printf.sprintf "Butanium_aoc/puzzle_input/day%d.txt" day

let get_test_lines () = readlines "Butanium_aoc/test.txt"