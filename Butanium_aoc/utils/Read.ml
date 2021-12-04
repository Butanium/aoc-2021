let readlines file = let ic = open_in file in 
let rec aux() = 
  try let s = input_line ic in s :: aux() with  
  End_of_file -> []
in aux()

let get_puzzle_lines day = 
  readlines @@ Printf.sprintf "puzzle_input/day%d.txt" day

let get_test_lines () = readlines "test.txt"


let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let get_puzzle day = 
  read_whole_file @@ Printf.sprintf "puzzle_input/day%d.txt" day
  