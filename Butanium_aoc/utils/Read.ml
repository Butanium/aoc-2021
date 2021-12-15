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

let get_test () = read_whole_file "test.txt"

let get_input_as_matrix lines = 
  let len1 = Array.length lines in
  Array.init len1 (fun i -> Base.String.to_array lines.(i))
let get_input_as_int_matrix lines = 
  let len1 = Array.length lines in
  let len2 = String.length lines.(0) in 
  Array.init len1 (fun i -> Array.init len2 (fun j -> let s = String.sub lines.(i) j 1  in 
      try int_of_string s with Failure e -> failwith @@ e ^ " : " ^ s))

let get_puzzle_matrix day = 
  let lines = Array.of_list @@ get_puzzle_lines day in 
  get_input_as_matrix lines

let get_puzzle_int_matrix day = 
  let lines = Array.of_list @@ get_puzzle_lines day in 
  get_input_as_int_matrix lines

let get_test_matrix () = 
  get_input_as_matrix @@ Array.of_list @@ get_test_lines ()

let get_test_int_matrix () =
  get_input_as_int_matrix @@ Array.of_list @@ get_test_lines ()