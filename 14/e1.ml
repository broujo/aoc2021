open Parser

let insert_one m c acc =
    match acc with
    | [] -> c :: acc
    | d :: _ -> begin
      match M.get (c, d) m with
      | None -> c :: acc
      | Some(i) -> c :: i :: acc
    end

let insert_at_once m l =
  CCList.fold_right (insert_one m) l []

let solve l =
  CCList.group_by l
  |> CCList.map (fun l -> CCList.length l)
  |> CCList.sort (CCInt.compare)
  |> (fun l -> (CCOpt.get_exn (CCList.last_opt l))-(CCList.hd l))

let test_input = "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"

let run input =
  let input_l,transform_map = parse input in
  CCFun.iterate 10 (insert_at_once transform_map) input_l
  |> solve

let () =
  assert ((run test_input) == 1588);
  let input = Helpers.input 14 in
  print_int (run (Lwt_main.run input))
