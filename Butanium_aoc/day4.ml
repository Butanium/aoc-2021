let puzzle = Read.get_puzzle 4

type board = {mutable numbers : int array array; chosens : bool array array}

let tirages_string, boards_string = match Str.split (Str.regexp_string "\r\n\r\n") puzzle with  | x :: xs -> x,xs | _ -> failwith "unexpected"

let boards = List.map (fun board -> {numbers = board; chosens = Array.make_matrix 5 5 false}) @@
    List.map (fun board_l -> Array.map Array.of_list @@ Array.of_list board_l) @@ 
    List.map (List.map (fun s -> List.map int_of_string @@ Str.split (Str.regexp " +") s)) @@ 
    List.map (Str.split (Str.regexp_string "\r\n")) boards_string

exception Win of board * int
exception Break

let check_win i j chosens = 
  Array.fold_left (&&) true chosens.(i) || Array.fold_left (fun acc x -> acc && x.(j)) true chosens

let update boards tirage = 
  List.iter (fun board -> try (
    Array.iteri (fun i l -> 
      Array.iteri (fun j x -> 
        if x = tirage then begin
          board.chosens.(i).(j) <- true; 
          if check_win i j board.chosens then raise @@ Win (board, tirage) else raise Break
        end
      ) l
    ) board.numbers 
  ) with Break -> ()
  ) boards

let tirages = List.map int_of_string @@ String.split_on_char ',' tirages_string

let get_score b t = let r = ref 0 in 
    Array.iteri (fun i l -> 
      Array.iteri (fun j x -> if not b.chosens.(i).(j) then r := !r + x) l ) 
    b.numbers; !r * t

let () = try (List.iter (update boards) tirages) with Win (win_board, win_tirage) -> 
  Printf.printf "result for day 4 part 1 : %d\n\n" @@ get_score win_board win_tirage

(* let () =  List.iter (fun b -> Array.iter (fun l -> Array.iter (Printf.printf "%b ") l; print_newline()) b.chosens; Printf.printf "\n\n") boards *)
let r = ref (-1)

let rec aux boards t = 
  try (update boards t) with Win (b, _) -> 
    begin
      r := get_score b t;
      Printf.printf "%d\n" t; 
      b.numbers <- [||]; 
      aux boards t
    end
let () = List.iter (aux boards) tirages;
  Printf.printf "result for day 4 part 2 : %d\n" !r