type s = { p1_score:int; p2_score:int; p1_pos:int; p2_pos:int; p1_turn:bool; rolled: int; dice: int CCSeq.t }

let possible_draws = CCList.product (fun (a,b) c -> a,b,c) (CCList.product (fun a b -> a,b) [1;2;3] [1;2;3]) [1;2;3]

let possible_values =
  possible_draws
  |> CCList.map (fun (a,b,c) -> a+b+c)
  |> CCList.group_by
  |> CCList.map (function | (t :: _) as l -> t, (CCList.length l) | [] -> failwith "should not be empty")


let thd (_,_,c) = c

let possible_scores pos n =
  let rec aux scores n =
    if n = 0 then scores
    else
      let scores =
        CCList.product (fun (score,pos,i) (draw,j) ->
          let pos = (pos+draw) mod 10 in
          let score = score + 1 + pos in
          (score, pos, i*j)) scores possible_values
        |> CCList.group_by
             ~hash:(fun (score,pos,_) -> Hashtbl.hash (score,pos))
             ~eq:(fun (score1,pos1,_) (score2,pos2,_) -> (score1,pos1) = (score2,pos2))
        |> CCList.map (function
          | [] -> failwith "no empty"
          | ((score,pos,_) :: _) as l -> (score, pos, (CCList.fold_left (+) 0 (CCList.map thd l))))
      in
      aux scores (n-1) in

  aux [(0,pos,1)] n

let play scores =
  CCList.product (fun (score,pos,i) (draw,j) ->
    let pos = (pos+draw) mod 10 in
    let score = score + 1 + pos in
    (score, pos, i*j)) scores possible_values
  |> CCList.group_by
      ~hash:(fun (score,pos,_) -> Hashtbl.hash (score,pos))
      ~eq:(fun (score1,pos1,_) (score2,pos2,_) -> (score1,pos1) = (score2,pos2))
  |> CCList.map (function
    | [] -> failwith "no empty"
    | ((score,pos,_) :: _) as l -> (score, pos, (CCList.fold_left (+) 0 (CCList.map thd l))))

let count_and_remove scores =
  let win,scores = CCList.partition_filter_map (fun ((score,_,i) as s) ->
    if score >= 21
    then `Left i
    else `Right s) scores in
  (CCList.fold_right (+) win 0),scores

let solve pos1 pos2 =

  let rec aux scores_pa scores_pb (pa_win, pb_win) =
    if CCList.is_empty scores_pa || CCList.is_empty scores_pb
    then (pa_win, pb_win)
    else
      let scores_pa = play scores_pa in
      let win,scores_pa = count_and_remove scores_pa in
      let universes_pb = CCList.fold_right (fun (_,_,n) acc -> n + acc) scores_pb 0 in
      let pa_win = pa_win + win * universes_pb in
      aux scores_pb scores_pa (pb_win, pa_win) in
  let pa,pb = aux [(0,pos1,1)] [(0,pos2,1)] (0,0) in
  max pa pb

let test_p1 = (4-1)
let test_p2 = (8-1)

let run = solve

let () =
  assert (run test_p1 test_p2 = 444356092776315);
  let input_p1 = (4-1) in
  let input_p2 = (6-1) in

  print_int (run input_p1 input_p2)
