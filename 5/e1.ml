open Parser

let rec gcd a b =
  if b = 0 then a
  else gcd b (a mod b);;

let get_dir p1 p2 =
  let dir = {x = (p2.x - p1.x); y = (p2.y - p1.y)} in
  let g = CCInt.abs (gcd dir.x dir.y) in
  {x = dir.x / g; y = dir.y / g} 

let make_p_list (p1,p2,dir) =
  let rec aux pi acc =
    match pi with
    | p when p = p2 -> p :: acc
    | p -> aux { x = p.x + dir.x; y = p.y + dir.y } (p :: acc) (* might run forever in case of mistakes *)
  in
  aux p1 []

let solve l =
  let open CCList in
  l
  |> filter (fun (p1,p2) -> p1.x = p2.x || p1.y = p2.y)
  |> map (fun (p1,p2) -> (p1,p2,(get_dir p1 p2)))
  >>= make_p_list
  |> group_by
  |> map (fun l -> length l)
  |> filter (fun s -> s > 1)
  |> length

let test_input =
  "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"

let run input =
  input
  |> parse
  |> solve
  

let () =
  assert ((run test_input) == 5);
  let input = Helpers.input 5 in
  print_int (run (Lwt_main.run input))
