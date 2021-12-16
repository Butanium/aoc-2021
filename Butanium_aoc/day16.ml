let convert_list = [("0","0000");
("1","0001");
("2","0010");
("3","0011");
("4","0100");
("5","0101");
("6","0110");
("7","0111");
("8","1000");
("9","1001");
("A","1010");
("B","1011");
("C","1100");
("D","1101");
("E","1110");
("F","1111")]
let puzzle = Read.get_puzzle 16
(* let puzzle = Read.get_test () *)

let convert puzzle = List.fold_left (fun s (to_r, bin)-> Str.global_replace (Str.regexp_string to_r) bin s) puzzle convert_list
let packets_string = convert puzzle
type packet_type = Literal of int | Operator of int * packet list and
packet = {version : int; packet : packet_type}
let print_packet packet = Printf.sprintf "version %d, %s %d\n" packet.version (match packet.packet with | Literal _ -> "Literal" | _ -> "Operator") (match packet.packet with | Literal x | Operator(x,_) -> x)
let int_of_bits s = 
  let rec aux acc pow pos = 
      if pos < 0 then acc else aux (acc + Util.int_of_char s.[pos] * pow) (pow*2) (pos - 1)
  in aux 0 1 @@ String.length s - 1

exception Parse_error of string
exception Parse_failure of string * packet list
exception End_of_message
type binary = B0 | B1
let to_b = function '0' -> B0 | '1' -> B1 | e -> raise @@ Parse_error (Printf.sprintf "read_bits : %c" e)
let rec parse limit position acc to_parse_string = 
  if position = String.length to_parse_string || limit = 0 then acc, position else begin
  let version = int_of_bits @@ String.sub to_parse_string position 3 in 
  let type_id =  int_of_bits @@ String.sub to_parse_string (position+3) 3 in
  let rec check_end last_pos = last_pos = String.length to_parse_string ||  
    match to_b @@ to_parse_string.[last_pos] with
      | B0 -> check_end (last_pos + 1)
      | B1 -> false
  in
  match type_id with 
    | 4 -> begin
      let rec aux_read pos = match to_b @@ to_parse_string.[pos] with
        | B0 -> String.sub to_parse_string (pos+1) 4, pos+5 
        | B1 -> let s, end_pos = aux_read (pos+5) in String.sub to_parse_string (pos+1) 4 ^ s, end_pos
      in 
      let bits_string, last_pos = aux_read (position + 6) in 
        let packet = Literal (int_of_bits bits_string) in 
        let new_packet = {version; packet} in 
        if check_end last_pos then new_packet :: acc, String.length to_parse_string else (
        Printf.printf "continue parsing after Literal with : %s\n%!" @@ 
        Base.String.suffix to_parse_string (String.length to_parse_string - last_pos);
        Printf.printf "added : %s" @@ print_packet new_packet;
        parse (limit-1) (last_pos) (new_packet :: acc) to_parse_string
        )
      end
    | o -> 
      let length_known = to_b @@ to_parse_string.[position+6] = B0 in
      let sub_length_length = if length_known then 15 else 11 in 
      let sub_packets_length = 
        let s = String.sub to_parse_string (position+7) sub_length_length in
        Printf.printf "sub_l_l : %s, int : %d\n" s (int_of_bits s); int_of_bits @@ s 
        in
        let sub_string = 
          String.sub to_parse_string (position+7+sub_length_length) sub_packets_length 
        in 
        let end_pos = position+7+sub_length_length in 
        if length_known then Printf.printf "sub parsing : %s of lenght %d \n%!" sub_string sub_packets_length else 
            Printf.printf "search %d sub packets in %s\n" sub_packets_length (String.sub to_parse_string end_pos (String.length to_parse_string - end_pos));
        let sub_packets, end_pos = 
          (if length_known then let s,_ = parse max_int 0 [] sub_string in s, end_pos + sub_packets_length else 
            parse sub_packets_length end_pos [] to_parse_string)
        in
        let packet = Operator (o, sub_packets) in 
        let new_packet = {version; packet} in 
        if check_end end_pos then new_packet :: acc, String.length to_parse_string  else (
          Printf.printf "continue parsing after operator with : %s, end pos : %d\n%!" (
          String.sub to_parse_string end_pos (String.length to_parse_string - end_pos) ) end_pos;
          Printf.printf "added : %s" @@ print_packet new_packet;

        parse (limit - 1) (end_pos) (new_packet :: acc) to_parse_string 
        )
    end
let r,_ = parse max_int 0 [] packets_string

let decoded_packet = 
  let r1,packet = let rec aux packet = packet.version + match packet.packet with 
    | Operator (_, packets) -> List.fold_left (fun acc p -> acc + aux p) 0 packets
    | _ -> 0
  in match parse max_int 0 [] packets_string  with | [x],_ -> aux x,x | l, _ -> failwith @@
    Printf.sprintf "result of parse with %d packets" @@ List.length l
  in
  Printf.printf "result for day 16 part 1 : %d\n" r1;packet


let rec eval p = match p.packet with 
  | Literal x -> x
  | Operator (op_type, sub_packets) -> 
    let comp_er () = raise @@ Parse_error "incorrect comparaison" in
    let op_f op = fun l -> (match l with [y;x] -> if op (eval x) (eval y) then 1 else 0 | _ -> comp_er()) in
    begin match op_type with 
    | 0 -> List.fold_left (fun acc packet -> acc + eval packet) 0
    | 1 -> List.fold_left (fun acc packet -> acc * eval packet) 1
    | 2 -> List.fold_left (fun acc packet -> min acc @@ eval packet) max_int
    | 3 -> List.fold_left (fun acc packet -> max acc @@ eval packet) (-1)
    | 5 -> op_f (>)
    | 6 -> op_f (<)
    | 7 -> op_f (=)
    | e -> raise @@ Parse_error (Printf.sprintf "unexpected operator : %d" e)
  end sub_packets

let part_2 = Printf.printf "result for day 16 part 2 %d" @@ eval decoded_packet
    