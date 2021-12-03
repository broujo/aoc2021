let binary_list_to_int =
  CCList.fold_left (fun x y -> (Int.shift_left x 1) + y) 0

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
  let cnt =
    input
    |> CCString.lines
    |> CCList.map CCString.to_list
    |> CCList.map (CCList.map (function '0' -> -1 | '1' -> 1 | _ -> 0))
    |> (function [] -> [] | h :: tl -> CCList.fold_right (CCList.map2 (+)) tl h)
  in
  let gamma = cnt
    |> CCList.map (fun x -> if x < 0 then 0 else 1)
    |> binary_list_to_int
  in let epsilon = cnt
    |> CCList.map (fun x -> if x > 0 then 0 else 1)
    |> binary_list_to_int
  in gamma * epsilon

let () =
  assert ((run test_input) == 198);
  let input = Helpers.input 3 in
  print_int (run (Lwt_main.run input))
