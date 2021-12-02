let get_puzzle_lines day = 
  let ic = open_in @@ Printf.sprintf "Butanium_aoc/puzzle_input/day%d.txt" day in 
  let rec aux() = 
    try let s = input_line ic in s :: aux() with  
    End_of_file -> []
  in aux()