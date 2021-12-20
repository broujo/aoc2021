let hex_to_bit c =
  match c with
  | '0' -> [0; 0; 0; 0]
  | '1' -> [0; 0; 0; 1]
  | '2' -> [0; 0; 1; 0]
  | '3' -> [0; 0; 1; 1]
  | '4' -> [0; 1; 0; 0]
  | '5' -> [0; 1; 0; 1]
  | '6' -> [0; 1; 1; 0]
  | '7' -> [0; 1; 1; 1]
  | '8' -> [1; 0; 0; 0]
  | '9' -> [1; 0; 0; 1]
  | 'A' -> [1; 0; 1; 0]
  | 'B' -> [1; 0; 1; 1]
  | 'C' -> [1; 1; 0; 0]
  | 'D' -> [1; 1; 0; 1]
  | 'E' -> [1; 1; 1; 0]
  | 'F' -> [1; 1; 1; 1]
  | _ -> []

let parse_number s l =
  let rec aux l s acc =
    match l,s with
    | _,0 -> acc,l
    | [],_ -> failwith "not enough input to parse an int of size s"
    | t::q,_ -> aux q (s-1) (acc*2 + t) in
  aux l s 0

let parse_version = parse_number 3
let parse_id = parse_number 3

let parse_litteral l =
  let rec aux l acc =
    match l with
    | [] -> failwith "cannot parse a littleral"
    | 0 :: q ->
        let n,q = parse_number 4 q in
        (acc*16 + n),q
    | 1 :: q ->
        let n,q = parse_number 4 q in
        aux q (acc*16 + n)
    | _ -> failwith "only 1 and 0"
  in aux l 0

let rec parse_operator l =
  let length, l, sub =
    match l with
    | 0 :: q -> 15, q, false
    | 1 :: q -> 11, q, true
    | _ -> failwith "only 1 and 0 or one missing" in
  let n, l = parse_number length l in
  if sub
  then
    let rec aux l n acc =
      if n = 0 then acc,l
      else let r,q = parse l in aux q (n-1) (r::acc) in
    aux l n []
  else
    let l1,l2 = CCList.take_drop n l in
    let rec aux l acc = (*fixme repeat parse until l1 is empty *)
      match l with
      | [] -> acc
      | _ -> let r,q = parse l in aux q (r::acc) in
    (aux l1 []),l2

and parse l =
  let _, l = parse_version l in
  let id, l = parse_id l in
  match id with
  | 4 -> let n,q = parse_litteral l in (n,q)
  | _ -> let r_l,q = parse_operator l in begin
    match id,r_l with
    | 0,_ -> (CCList.fold_right (+) r_l 0), q
    | 1,_ -> (CCList.fold_right ( * ) r_l 1), q
    | 2,_ -> (CCList.fold_right min r_l CCInt.max_int), q
    | 3,_ -> (CCList.fold_right max r_l CCInt.min_int), q
    | 5,a::b::[] -> if a < b then 1,q else 0,q
    | 6,a::b::[] -> if a > b then 1,q else 0,q
    | 7,a::b::[] -> if a = b then 1,q else 0,q
  | _ -> failwith "impossible case" end

let test_input_1 = "C200B40A82"
let test_input_2 = "04005AC33890"
let test_input_3 = "880086C3E88112"
let test_input_4 = "CE00C43D881120"
let test_input_5 = "D8005AC2A8F0"
let test_input_6 = "F600BC2D8F"
let test_input_7 = "9C005AC2F8F0"
let test_input_8 = "9C0141080250320F1802104A08"


let run input =
  input
  |> CCString.to_list
  |> CCList.flat_map hex_to_bit
  |> parse
  |> fst


let () =
  assert ((run test_input_1) = 3);
  assert ((run test_input_2) = 54);
  assert ((run test_input_3) = 7);
  assert ((run test_input_4) = 9);
  assert ((run test_input_5) = 1);
  assert ((run test_input_6) = 0);
  assert ((run test_input_7) = 0);
  assert ((run test_input_8) = 1);
  let input = Helpers.input 16 in
  print_int (run (Lwt_main.run input))
