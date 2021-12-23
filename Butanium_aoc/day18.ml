(* DISCLAIMER : this method is not the one I should have used. Instead prefer working with the string, 
it'll be way easier to accesss to the right and left elements *)

let str_numbers = Read.get_puzzle_lines 18
(* let str_numbers = Read.get_test_lines () *)
type snailfish_mut = N of {mutable left: node ; mutable right: node } | I of int 
and node = {content: snailfish_mut; mutable depth: int; mutable parent: parent} and parent = Root | Left of node | Right of node

(* let depth_chars = [|[|"|";"|";"()"; "{|}"; "!"; "@"|]|];[|"|";"|";"()"; "{|}"; "!"; "@"|]|] *)
let depth_chars = [|[|"[";"[";"[";"[";"{";"{";|]; [|"]";"]";"]";"]";"}";"}" |]|]

let rec debug_tree node = match node.content with 
  | I i -> string_of_int i 
  | N n ->   Printf.sprintf "%s%s,%s%s" depth_chars.(0).(node.depth) (debug_tree n.left) (debug_tree n.right) depth_chars.(1).(node.depth) 
let split_middle str =
  let rec aux pos acc = match str.[pos] with
    | ',' when acc = 1 -> String.sub str 1 (pos-1), Util.end_string ~ignore_end:1 str (pos+1)
    | '[' -> aux (pos+1) (acc+1)
    | ']' -> aux (pos+1) (acc-1)
    | _ -> aux (pos+1) acc
  in aux 0 0 

let rec parse_number depth nb_string  = 
  if String.length nb_string = 1 then 
    {content = I (int_of_string nb_string); depth; parent=Root}
  else
    let l, r = split_middle nb_string in 
      {content = N {left=parse_number (depth+1) l; right = parse_number (depth+1) r}; depth; parent=Root}

let rec update_all_parents parent node =
    node.parent <- parent;
    match node.content with 
    | N n -> update_all_parents (Right node) n.right; update_all_parents (Left node) n.left
    | _ -> ()
let get_numbers () = let r = List.map (parse_number 0) str_numbers in 
  List.iter (update_all_parents Root) r; r
let numbers = get_numbers ()
let rec magnitude = function
  | I i -> i
  | N {left; right} -> 3 * magnitude left.content + 2 * magnitude right.content
exception Explosion of int*int

let split_n n = if n mod 2 = 0 then n/2, n/2 else n/2, n/2+1

let get_ints node =
  let get_int n = match n.content with 
    | I i -> i
    | _ -> raise @@ Invalid_argument "can't get int of a node"
  in 
  match node.content with 
  | N e -> get_int e.left, get_int e.right
  | _ -> raise @@ Invalid_argument "can't get ints of a non int*int couple'"

let get_left_child = function {content=N n; _} -> n.left | _ -> raise @@ Invalid_argument "no left child for I"
let set_left_child repl = function {content=N n; _} -> n.left <- repl | _ -> raise @@ Invalid_argument "no left child for I"
let set_right_child repl = function {content = N n; _} -> n.right <- repl | _ -> raise @@ Invalid_argument "no right child for I"
let get_right_child = function {content = N n; _} -> n.right | _ -> raise @@ Invalid_argument "no right child for I"
let set_child repl = function
  | Root -> raise @@ Invalid_argument "can't set a child of a root"
  | Left n -> set_left_child repl n
  | Right n -> set_right_child repl n
let get_parent = function | Left n | Right n -> n | Root -> raise @@ Invalid_argument "can't get parent of a root"
let rec get_root node = match node.parent with | Root -> node | _ -> get_root @@ get_parent node.parent
let debug_root node = (debug_tree @@ get_root node)
let rec check_split to_split_node = 
  match to_split_node.content with 
  | I i -> if i < 10 then false else begin
    (* Printf.printf "splitting %d in %s\n\n" i (debug_root to_split_node); *)
    let l, r = split_n i in
    let depth = to_split_node.depth + 1 in 
    let left = {content= I l; depth; parent=Root} in
    let right = {content=I r; depth; parent=Root} in 
    let content = N{left; right} in
    let new_node = {content; depth=to_split_node.depth; parent = to_split_node.parent} in 
    update_all_parents to_split_node.parent new_node;
    let () = set_child new_node to_split_node.parent in
    if to_split_node.depth >= 4 then (
      explode new_node
    );
    true
  end
  | _ -> raise (Invalid_argument "can't split node if it's not an integer")

and add_left amount node = match node.content with 
  | I i -> let new_node = {content = I (i+amount); depth = node.depth; parent = node.parent} in (
            try set_child new_node node.parent with Invalid_argument e -> failwith (e ^ ": " ^ "add_left") )
  | N {left; _} -> add_left amount left
and add_right amount node = match node.content with 
  | I i -> let new_node = {content = I (i+amount); depth = node.depth; parent = node.parent} in 
            (try set_child new_node node.parent with Invalid_argument e -> failwith (e ^ ": " ^ "add_right"))
  | N {right; _} -> add_right amount right
and go_up_left amount node =
  match node.parent with
  | Left n -> go_up_left amount n
  | Right n -> add_right amount @@ get_left_child n
  | Root -> ()
and go_up_right amount node =
  match node.parent with 
  | Root -> ()
  | Right n -> go_up_right amount n
  | Left n -> add_left amount @@ get_right_child n 

and explode to_explode_node = 
  assert (to_explode_node.depth >= 4);
  (* Printf.printf "exploding %s in %s\n\n" (debug_tree to_explode_node) (debug_root to_explode_node); *)
  let left, right = get_ints to_explode_node in 
  set_child  {content=I 0; depth = 4; parent=to_explode_node.parent} to_explode_node.parent;
  go_up_left left @@ to_explode_node;
  go_up_right right @@ to_explode_node


let is_I = function | I _ -> true | _ -> false
exception Node_error of string*node
let rec search_explode node = 
  match node.content with
  | I _ -> false
  | N n -> if node.depth = 4 && is_I n.right.content && is_I n.left.content then (
      (* if not () then raise @@ Node_error ("ERROR : this node is at depth 4 : " ^debug_tree node ^ "\n"^debug_root node,node);  *)
      explode node; true) else (search_explode n.left || search_explode n.right)

let rec search_split node = match node.content with 
  | I _ -> check_split node
  | N n -> search_split n.left || search_split n.right
  
let rec reduce number = 
  (* Printf.printf "\nreducing : %s\n\n" @@ debug_tree number; *)
  (search_explode number || search_split number) && reduce number

let rec increase_depth ?(amount=1) node =
  node.depth <- node.depth + amount;
  match node.content with
  | N x -> increase_depth ~amount x.left; increase_depth ~amount x.right
  | _ -> ()


let (+@) n1 n2 = 
  let new_node = {content=N {left=n1; right=n2}; depth=0; parent=Root} in 
  n1.parent <- Left new_node;
  n2.parent <- Right new_node;
  increase_depth n1;
  increase_depth n2; new_node
let (++) n1 n2 = 
  let new_node = n1 +@ n2 in 
  let _ = reduce new_node in new_node

let added numbers = List.fold_left (++) (List.hd numbers) (List.tl numbers)
let debug t = let _ = reduce t in Printf.printf "tree : %s\n" (debug_tree t)
let part1 () = let r = added numbers in debug r;
  Printf.printf "\nresult for day 18 part 1 : %d\n" 
  (magnitude r.content)

let part2  = 
  let arr = Array.of_list numbers in 
  let len = Array.length arr in 
  let r = ref 0 in 
  for i = 0 to len - 1 do
    for j = 0 to len - 1 do
        if i <> j then begin 
        let arr = Array.of_list @@ get_numbers() in 
        let len =  magnitude (arr.(i)++arr.(j)).content in 
        Printf.printf "adding %d and %d with a result of %d\n%!" i j len;
        r := max !r len
        end 
    done
  done;
  Printf.printf "\nresult for day 18 part 2 : %d" !r

