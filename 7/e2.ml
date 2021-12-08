let sort_by_freq l =
  let open CCList in
  l
  |> group_by
  |> map (function | [] -> (0,0) | t :: q -> (t, 1 + length q))
  |> sort (fun (_,a) (_,b) -> Int.compare b a) 

let compute_fuel k base =
  let rec aux b n acc =
    match b with
    | 0 -> acc
    | _ -> aux (b-1) (n+1) (acc + n)
  in aux (Int.abs (base - k)) 1 0

let solve l =
  let rec find_with_base l base curmin acc =
    if acc >= curmin
    then CCInt.max_int
    else begin
      match l with
      | [] -> acc
      | (k,x) :: q -> find_with_base q base curmin (acc + (compute_fuel k base) * x)
    end in
  let rec aux starti endi min =
    match starti with
    | s when s = endi -> min
    | s ->
        let r = find_with_base l s min 0 in
        aux (s+1) endi (CCInt.min r min) in
  aux
    (CCList.fold_right (fun (k1,_) m -> CCInt.min k1 m) l CCInt.max_int)
    (CCList.fold_right (fun (k1,_) m -> CCInt.max k1 m) l 0)
    CCInt.max_int

let test_input = "16,1,2,0,4,2,7,1,2,14"

let run input =
  input
  |> CCString.trim
  |> CCString.split ~by:","
  |> CCList.map (CCInt.of_string_exn)
  |> sort_by_freq
  |> solve
  |> (fun i -> print_int i; print_string "\n"; i)

let () =
  assert ((run test_input) == 168);
  let input = Helpers.input 7 in
  print_int (run (Lwt_main.run input))

