let solve input =
  let generic input cmp =
    let rec aux input acc =
      match input with
      | [] -> failwith "input should not be empty"
      | h :: [] -> (CCList.rev h) @ acc
      | h :: tl ->
          let cnt = CCList.fold_right (CCList.map2 (+)) tl h
          in match cnt with
          | [] -> acc
          | t :: _ ->
              let nbit =
                if cmp t 0
                then 1
                else -1 in
              let input =
                input
                |> CCList.filter (fun l -> (CCList.hd l) = nbit) (* hd is ok, cnt was matched *)
                |> CCList.map CCList.tl
              in aux input (nbit :: acc)
    in aux input []
      |> CCList.map (fun x -> if x < 0 then 0 else 1)
      |> (fun l -> CCList.fold_right (fun x y -> (Int.shift_left y 1) + x) l 0)
  in let oxygen = generic input (>=)
  in let scrubber = generic input (<)
  in oxygen * scrubber


let test_input = "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"


let run input =
  input
  |> CCString.lines
  |> CCList.map CCString.to_list
  |> CCList.map (CCList.map (function '0' -> -1 | '1' -> 1 | _ -> 0))
  |> solve

let () =
  assert ((run test_input) = 230);
  let input = Helpers.input 3 in
  print_int (run (Lwt_main.run input))
