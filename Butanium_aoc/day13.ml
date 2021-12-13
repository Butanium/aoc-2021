let puzzle = Read.get_puzzle 13
(* let puzzle = Read.get_test () *)
let puzzle = Str.(global_replace (regexp "fold along ") "" @@ puzzle) 

let dots, instructions = match Str.split (Str.regexp_string "\r\n\r\n") puzzle with 
  | x :: y :: [] -> x,y | _ -> failwith "parse failure"


let dots = List.map (fun s -> Scanf.sscanf s "%d,%d" (fun x y -> x,y)) @@ Read.split_on_newline dots

(* let max_x, max_y = List.fold_left (fun (max_x,max_y) (x,y) -> 
    max max_x x, max max_y y) (0,0) dots *)

type instruction = Horizontal_fold of int | Vertical_fold of int
type paper = Empty | Full
let to_string = function Empty -> "." | Full -> "#"
let is_horizontal_fold = function Horizontal_fold _ -> true | Vertical_fold _ -> false
let get_coord = function Horizontal_fold x -> x | Vertical_fold y -> y 
let instructions = List.map (fun s -> Scanf.sscanf s "%c=%d" (fun s d -> 
    match s with 'x' -> Horizontal_fold d | 'y' -> Vertical_fold d | _ -> failwith "parse failure instructions")) @@ Read.split_on_newline instructions

type paper_sheet = {mutable sizey : int; mutable sizex : int; paper : paper array array}
let paper_sheet = 
  let sizex, sizey = 2 * (get_coord @@ List.find is_horizontal_fold instructions) + 1, 
                       2 * (get_coord @@ List.find (fun x -> not @@ is_horizontal_fold x) instructions) + 1
  in
  let paper = Array.make_matrix sizey sizex Empty in 
  List.iter (fun (x,y) -> paper.(y).(x) <- Full) dots;
  {sizey; sizex; paper}

let (|+) x y = match x,y with | Empty, Empty -> Empty | _ -> Full
let perform_fold = function 
  | Horizontal_fold x -> assert (2*x + 1 = paper_sheet.sizex); 
      for ix = x to paper_sheet.sizex - 1 do
        for y = 0 to paper_sheet.sizey - 1 do
          paper_sheet.paper.(y).(2*x - ix) <- paper_sheet.paper.(y).(ix) |+ paper_sheet.paper.(y).(2*x - ix);
          paper_sheet.paper.(y).(ix) <- Empty

        done
      done;
      paper_sheet.sizex <- x 
  | Vertical_fold y -> assert (2*y + 1 = paper_sheet.sizey);
      for iy = y to paper_sheet.sizey - 1 do
        for x = 0 to paper_sheet.sizex - 1 do
          paper_sheet.paper.(2*y - iy).(x) <- paper_sheet.paper.(iy).(x) |+ paper_sheet.paper.(2*y - iy).(x);
          paper_sheet.paper.(iy).(x) <- Empty
        done
      done;
      paper_sheet.sizey <- y

let debug_paper () = 
  for y = 0 to paper_sheet.sizey - 1 do
    for x = 0 to paper_sheet.sizex - 1 do
       print_string @@ to_string paper_sheet.paper.(y).(x)
    done;
    print_newline ()
  done

let rec iter_paper = function 
  | [] -> ()
  | x :: xs -> perform_fold x; iter_paper xs

let get_dot_count () = 
    Util.matrix_fold (fun acc -> function | Empty -> acc | Full -> acc + 1) 0 paper_sheet.paper


let () = match instructions with 
  | [] -> () 
  | x :: xs -> iter_paper [x]; Printf.printf "\nresult for day 13 part 1 : %d\n" @@ get_dot_count();
    iter_paper xs; Printf.printf "result for day 13 part 2 : \n\n";
    debug_paper()
