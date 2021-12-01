let check_succ l =
  let rec aux l acc =
    match l with
    | [] | [_] | [_; _] | [_; _; _] -> acc
    | x :: a :: b :: y :: tl ->
        if y > x
        then aux (a :: b :: y :: tl) (acc+1)
        else aux (a :: b :: y :: tl) acc
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
  assert ((run test_input) == 5);
  let input = Helpers.input 1 in
  print_int (run (Lwt_main.run input))

