type s = { p1_score:int; p2_score:int; p1_pos:int; p2_pos:int; p1_turn:bool; rolled: int; dice: int CCSeq.t }

let step ({ p1_score; p2_score; p1_pos; p2_pos; p1_turn; rolled; dice}) =
  let roll = CCSeq.take 3 dice in
  let rolled = rolled + 3 in
  let dice = CCSeq.drop 3 dice in
  let roll_sum = CCSeq.fold (fun acc r -> acc+r) 0 roll in

  if p1_turn then
    let p1_pos = (p1_pos + roll_sum) mod 10 in
    let p1_score = p1_score + p1_pos + 1 in
    let s = { p1_score; p1_pos; p2_pos; p2_score; p1_turn=false; rolled; dice} in
    Some(s,s)
  else
    let p2_pos = (p2_pos + roll_sum) mod 10 in
    let p2_score = p2_score + p2_pos + 1 in
    let s = { p1_score; p1_pos; p2_pos; p2_score; p1_turn=true; rolled; dice} in
    Some(s,s)

(* Seq.find - only available in 4.14 *)
let rec find p xs =
  match xs() with
  | CCSeq.Nil ->
      None
  | CCSeq.Cons (x, xs) ->
      if p x then Some x else find p xs

let game = CCSeq.unfold step

let dice = CCSeq.cycle (CCSeq.range 1 100)

let test = {p1_score=0; p2_score=0; p1_pos=(4-1); p2_pos=(8-1); p1_turn=true; rolled=0; dice=dice}

let run input =
  let game = game input in
  match find (fun s -> s.p1_score >= 1000 || s.p2_score >= 1000) game with
  | None -> failwith "no result"
  | Some(s) -> (min s.p1_score s.p2_score) * s.rolled
  
let () =
  assert (run test = 739785);
  let input = {p1_score=0; p2_score=0; p1_pos=(4-1); p2_pos=(6-1); p1_turn=true; rolled=0; dice=dice} in
  print_int (run input)
