let sort_by_freq l =
  let open CCList in
  l
  |> group_by
  |> map (function | [] -> (0,0) | t :: q -> (t, 1 + length q))
  |> sort (fun (_,a) (_,b) -> Int.compare b a) 

let solve l =
  let rec find_with_base l base acc =
    match l with
    | [] -> acc
    | (k,x) :: q -> find_with_base q base (acc + (Int.abs (base - k)) * x) in
  let rec aux l acc min =
    match l with
    | [] -> min
    | (k,x) :: q ->
        let r = find_with_base (acc @ q) k 0 in
        if r <= min
        then aux q ((k,x)::acc) r
        else aux q ((k,x)::acc) min in
  aux l [] CCInt.max_int

let test_input = "16,1,2,0,4,2,7,1,2,14"

let run input =
  input
  |> CCString.trim
  |> CCString.split ~by:","
  |> CCList.map (CCInt.of_string_exn)
  |> sort_by_freq
  |> solve

let () =
  assert ((run test_input) == 37);
  let input = Helpers.input 7 in
  print_int (run (Lwt_main.run input))

