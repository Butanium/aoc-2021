let lines = Read.get_puzzle_lines 10
(* let lines = Read.get_test_lines () *)
let get_op = function
  | '(' -> ')'
  | ')' -> '('
  | '{' -> '}'
  | '}' -> '{'
  | '[' -> ']'
  | ']' -> '['
  | '>' -> '<'
  | '<' -> '>'
  | e -> failwith @@ Printf.sprintf "unexpected char : %c" e

let is_start = function | ')' |']' | '}' | '>'  -> false | _ -> true
type 'a line_type = Corrupted of 'a | Missing of 'a list | Valid  
let check line = 
  let stack = Stack.create () in 
  let rec aux = function
    | [] -> if Stack.is_empty stack then Valid else Missing (List.of_seq @@ Stack.to_seq stack) 
    | x :: xs -> if is_start x then (Stack.push x stack; aux xs) else (
                  if Stack.is_empty stack then 
                    Missing [get_op x] 
                  else if Stack.pop stack = get_op x then 
                      aux xs else Corrupted x
                 ) 
  in aux @@ Base.String.to_list line

let checked_lines = List.map check lines

let () = 
  let table = Hashtbl.create 4 in 
  Hashtbl.add_seq table (List.to_seq [')',0 ;']',0 ; '}',0 ; '>',0]);
  List.iter (function | Corrupted x -> Hashtbl.replace table x @@ 1 + Hashtbl.find table x | _ -> ()) checked_lines;
  let part1_r = Hashtbl.fold (fun chr amount acc -> acc + 
          (match chr with 
          | ')' -> 3
          | ']' -> 57
          | '}' -> 1197
          | '>' -> 25137
          | _ -> failwith @@ Printf.sprintf "invalid char : %c" chr)
          *amount 
        ) table 0
  in Printf.printf "\nresult for day 10 part 1 : %d\n\n" part1_r

let get_complete_score = function 
  | Missing l -> List.fold_left 
      (fun acc x -> acc * 5 +
        match x with | '(' -> 1 | '[' -> 2 | '{' -> 3 | '<' -> 4 | _ -> failwith "invalid char"
      ) 0 l
  | _ -> -1

let scores = List.sort compare @@ List.fold_left 
    (fun acc x -> let s = get_complete_score x in  if s >= 0 then s :: acc else acc) [] checked_lines 
let () = 
      Printf.printf "result for day 10 part 2 : %d" @@ List.nth scores (List.length scores / 2)
