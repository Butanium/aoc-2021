let couple_op op (x,y) (a,b)= op x a,op y b 
let couple_map op (x,y) = op x, op y
let mapi_in_place f arr = 
  for i = 0 to Array.length arr - 1 do
    arr.(i) <- f i arr.(i)
  done

let mapi_matrix_in_place f mat =
  Array.iteri (fun i x -> mapi_in_place (f i) x) mat

let bound xbound ybound (x,y) = let bound b coord =  0 <= coord && coord <= b in bound xbound x && bound ybound y

let get_neigbours ?(allow_diags = true) p xbound ybound =
  let bound = bound xbound ybound in
  let (++) = couple_op (+) in
  List.filter bound @@ List.map (fun x -> x ++ p) 
    ((if allow_diags then [1,1 ; 1,-1 ; -1,1 ; -1,-1] else []) @ [0,1 ; 0,-1 ; 1,0 ; -1,0])

