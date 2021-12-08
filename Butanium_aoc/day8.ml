module CharSet = Set.Make (Char)
module CharSetSet = Set.Make(CharSet)
let split = String.split_on_char ' ' 
let input = Read.get_puzzle_lines 8
(* let input = Read.get_test_lines() *)
let inputs = List.map   
  (fun s -> match String.split_on_char '|' s with 
    | [s1; s2] -> split @@ String.trim s1, split @@ String.trim s2 
    | _ -> failwith @@ "string parse failure") @@  input

let get_1_4_7_8 = List.fold_left (fun acc x -> match String.length x with | 2 | 3 | 4 | 7 -> acc+1 | _ -> acc) 0

let () = Printf.printf "\nResult for day 8, part 1 : %d\n" @@ 
    List.fold_left (fun acc (_,x) -> acc + get_1_4_7_8 x) 0 inputs

let map_to_set l = List.map (fun s -> CharSet.of_list @@ Base.String.to_list s) l
let set_inputs = List.map (fun (l1, l2) -> CharSetSet.of_list @@ map_to_set l1, map_to_set l2) inputs
(* sizes of digits
0 : 6
1 : 2 *
2 : 5
3 : 5 
4 : 4 *
5 : 5 
6 : 6
7 : 3 *
8 : 7 *
9 : 6
*)
let decode input = 
  let numbers = Array.make 10 CharSet.empty in 
  let to_decode, code = input in  
  let to_decode = ref to_decode in
  let find_first p s = 
    List.find p (CharSetSet.elements s)
  in
  let find_delete p = 
    let r = find_first p !to_decode in
    to_decode := CharSetSet.remove r !to_decode;
    r
  in 
  let init = CharSet.of_list @@ Base.String.to_list "abcdefg" in 
  let f_char = Option.get @@ CharSetSet.fold (fun x acc -> match acc with | Some a -> Some a 
  | None -> CharSet.choose_opt @@ CharSetSet.fold CharSet.inter (CharSetSet.remove x !to_decode) init) !to_decode None
  in 
  let not_mem c = fun x -> not @@ CharSet.mem c x in 
  assert (1=CharSetSet.cardinal @@ CharSetSet.filter (not_mem f_char) !to_decode);
  numbers.(2) <- find_delete @@ not_mem f_char;
  let size_equal size = (fun x -> CharSet.cardinal x = size) in 
  numbers.(1) <- find_delete @@ size_equal 2;
  numbers.(4) <- find_delete @@ size_equal 4;
  numbers.(7) <- find_delete @@ size_equal 3;
  numbers.(8) <- find_delete @@ size_equal 7;
  let c_char = CharSet.choose @@ CharSet.remove f_char numbers.(1) in 
  let no_c_num = CharSetSet.filter (not_mem c_char) !to_decode in
  let set5 = find_first (size_equal 5) no_c_num in 
  let set6 = find_first (size_equal 6) no_c_num in 
  let equal e = fun x -> x=e in 
  numbers.(5) <- find_delete (equal set5);
  numbers.(6) <- find_delete (equal set6);
  numbers.(3) <- find_delete (size_equal 5);
  numbers.(9) <- find_delete (fun x -> size_equal 4 @@ CharSet.inter numbers.(4) x);
  numbers.(0) <- find_delete @@ Fun.const true;
  assert (CharSetSet.cardinal !to_decode = 0);
  let find_n set = 
    let rec aux i = 
      if CharSet.equal set numbers.(i) then i else aux (i+1)
    in aux 0
  in
  let rec aux times acc = function 
    | [] -> acc
    | x :: xs -> aux (times*10) (acc + find_n x *times) xs
  in aux 1 0 @@ List.rev code 

let () = Printf.printf "%d\n" @@ List.fold_left (fun acc x -> acc + decode x) 0 set_inputs