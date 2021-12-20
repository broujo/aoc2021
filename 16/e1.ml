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
      else let v,q = parse l in aux q (n-1) (acc+v) in
    aux l n 0
  else
    let l1,l2 = CCList.take_drop n l in
    let rec aux l acc = (*fixme repeat parse until l1 is empty *)
      match l with
      | [] -> acc
      | _ -> let v, q = parse l in aux q (v + acc) in
    (aux l1 0),l2

and parse l =
  let version, l = parse_version l in
  let id, l = parse_id l in
  match id with
  | 4 -> let _,q = parse_litteral l in (version,q)
  | _ -> let v,q = parse_operator l in ((v + version),q)

let test_input_1 = "D2FE28"
let test_input_1bis = "38006F45291200"
let test_input_2 = "8A004A801A8002F478"
let test_input_3 = "620080001611562C8802118E34"
let test_input_4 = "C0015000016115A2E0802F182340"
let test_input_5 = "A0016C880162017C3686B18A3D4780"


let run input =
  input
  |> CCString.to_list
  |> CCList.flat_map hex_to_bit
  |> parse
  |> fst


let () =
  assert ((run test_input_1) = 6);
  assert ((run test_input_1bis) = 9);
  assert ((run test_input_2) = 16);
  assert ((run test_input_3) = 12);
  assert ((run test_input_4) = 23);
  assert ((run test_input_5) = 31);
  let input = Helpers.input 16 in
  print_int (run (Lwt_main.run input))
