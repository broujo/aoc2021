let check_succ l =
  let rec aux l acc =
    match l with
    | [] | [_] -> acc
    | x :: y :: tl ->
        if y > x
        then aux (y :: tl) (acc+1)
        else aux (y :: tl) acc
  in aux l 0

let test_input = "199
200
208
210
200
207
240
269
260
263"

let run input = 
  input
  |> CCString.lines
  |> CCList.map int_of_string
  |> check_succ

let () =
  assert ((run test_input) == 7);
  let input = Helpers.input 1 in
  print_int (run (Lwt_main.run input))

