let player1Start = 2
let player2Start = 10

let init_dice () = 
  let i = ref 1 in
  fun () -> if !i = 100 then (i := 1; 100) else (i := !i + 1; !i-1)

let get_three dice = dice () + dice () + dice ()

let to_pos = function 0 -> 10 | x -> x
let rec play p1_score p2_score p1_pos p2_pos dice turn =
  if p1_score >= 1000 || p2_score >= 1000 then begin 
    (* Printf.printf "loose : %d, win : %d, turn : %d, dices : %d" *)
    (min p1_score p2_score * turn * 3) 
  end else
  let forward = get_three dice in 
  let new_pos = to_pos ((p1_pos + forward) mod 10) in 
  play p2_score (p1_score+new_pos) p2_pos new_pos dice (turn+1)

let part1 = Printf.printf "result for day 21 part 1 : %d\n%!" @@ play 0 0 player1Start player2Start (init_dice()) 0

let (++) = Util.couple_op (+)
let ($) t (a,b) = a*t, b*t  
let score_table = Hashtbl.create 10 
let rec possibilities  = function
    | [a;b;c] -> Util.incr_hashtable score_table (a+b+c)
    | l -> List.iter (fun x -> possibilities (x::l)) [1;2;3]

let init_score_table = possibilities []

let rev (a,b) = (b, a)
let revifnot p t  = if p then t else rev t 
let quantum_wins max = 
  let win_table = Hashtbl.create 10000 in
  let debug = ref 1000 in 
  let add_win (p1_info,p2_info) is_p1_turn r = 
    Hashtbl.add win_table ((p1_info, p2_info), is_p1_turn) r;
    Hashtbl.add win_table ((p2_info, p1_info), not is_p1_turn) @@ rev r;
    if !debug = 1000 then (debug := 0; Printf.printf "win_table size : %d\n%!" (Hashtbl.length win_table));
  incr debug

  in
  let rec play ((play_score,play_pos) as playing_p) ((waiting_score,_) as waiting_p) is_p1_turn = 
    if waiting_score >= max then revifnot is_p1_turn (0,1) else begin
    match Hashtbl.find_opt win_table (revifnot is_p1_turn (playing_p, waiting_p), is_p1_turn) with 
    | Some r -> r
    | None -> 
      let r = Hashtbl.fold 
      (fun sum times acc -> 
        acc ++ (
        times $ 
          let pos = to_pos ((play_pos+sum) mod 10) in 
          play waiting_p (play_score+pos, pos) (not is_p1_turn)
        )
      ) score_table (0,0) in
      add_win (revifnot is_p1_turn (playing_p, waiting_p)) is_p1_turn r;
      r
    end
  in 
  let r = play (0, player1Start) (0, player2Start) true in r

let test = Hashtbl.iter (fun sum times -> Printf.printf "score of %d appears %d times\n" sum times) score_table
let part2 = 
  let start_time = Sys.time () in 
  let p1, p2 = quantum_wins 21 in 
   Printf.printf "results : \n  - p1 : %d wins\n  - p2 : %d wins\nThis beauty ran in %g seconds !!!" p1 p2 
   (Sys.time () -. start_time)