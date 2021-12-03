let lines = Read.get_puzzle_lines 3 in 
let conv = function | '0' -> -1 | '1' -> 1 | _ -> failwith "unexpected" in 
let arr = Array.make 12 0 in 
let add arr i c  = arr.(11-i) <- arr.(11-i) + conv c in
List.iter (String.iteri @@ add arr) lines;
let i, r = ref 1, ref 0 in
let aux m x = r := !r + (if x*m > 0 then 1 else 0) * !i; i := 2* !i in 
let f m = Array.iter (aux m) arr; !r in 
let gamma = f 1 in 
r:=0; i:= 1;
let epsilon = f (-1) in 
Printf.printf "result for day 3 part 1 : %d, gamma : %d, epsilon : %d\n" (gamma*epsilon) gamma epsilon;

let to_int c = int_of_char c - int_of_char '0' in 
let rec aux acc pow i m = function 
  | [s] as l ->print_endline s; if String.length s = i then acc else aux (acc+ pow * to_int s.[i]) (pow/2) (i+1) m l
  | [] -> failwith @@ Printf.sprintf "%d i, %d m, %d pow, %d acc" i m pow acc
  | l -> let x = List.fold_left (fun acc' x -> acc' + conv x.[i]) 0 l in
          let chosen =  if x*m > 0 then 1 else if x = 0 then (m + 1)/2 else 0 in 
          let l2 = List.filter (fun s ->  to_int s.[i] =  chosen) l in 
          aux (acc + pow * chosen) (pow/2) (i+1) m l2
in
let f = aux 0 (int_of_float @@ 2. ** 11.) 0 in 
let gamma = f 1 lines in 
let epsilon = f (-1) lines in 
Printf.printf "result for day 3 part 2 : %d, gamma : %d, epsilon : %d\n" (gamma*epsilon) gamma epsilon;
