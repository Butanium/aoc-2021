let couple_op op (x,y) (a,b)= op x a,op y b 
let couple_map op (x,y) = op x, op y
let mapi_in_place f arr = 
  for i = 0 to Array.length arr - 1 do
    arr.(i) <- f i arr.(i)
  done

let mapi_matrix_in_place f mat =
  Array.iteri (fun i x -> mapi_in_place (f i) x) mat

let init_matrix sizey sizex f = 
  Array.init sizey (fun i -> Array.init sizex (fun j -> f i j))

let matrix_fold f start matrix = 
  Array.fold_left (fun acc x -> Array.fold_left f acc x) start matrix

let bound xbound ybound (x,y) = 
  let bound b coord =  0 <= coord && coord <= b in 
  bound xbound x && bound ybound y

let get_neighbours ?(allow_diags = true) xbound ybound p=
  let bound = bound xbound ybound in
  let (++) = couple_op (+) in
  List.filter bound @@ List.map (fun x -> x ++ p) 
    ((if allow_diags then [1,1 ; 1,-1 ; -1,1 ; -1,-1] else []) @ [0,1 ; 0,-1 ; 1,0 ; -1,0])

let split_parts = Str.split (Str.regexp_string "\r\n\r\n")
let split_on_newline = Str.split (Str.regexp_string "\r\n")
let get_or_default table key default = match Hashtbl.find_opt table key with Some x -> x | None -> default

let add_hashtable amount table key  = Hashtbl.replace table key @@ get_or_default table key 0 + amount

let incr_hashtable table key = add_hashtable 1 table key

let int_of_char x = 
  try 
    int_of_string @@ Char.escaped x
  with Failure e -> raise @@ Failure (Printf.sprintf "%s : %c n'est pas un chiffre" e x) 

let map_matrix f mat = 
  Array.map (fun arr -> Array.map f arr) mat

let iteri_matrix f =
  Array.iteri (fun y arr -> Array.iteri (fun x e -> f y x e) arr)