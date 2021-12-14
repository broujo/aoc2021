open Parser

module C = struct
  type t = Int.t * Int.t
  let compare (x0,y0) (x1,y1) =
    match Int.compare x0 x1 with
    | 0 -> Int.compare y0 y1
    | c -> c
end

module S = CCSet.Make(C)

let do_one_fold dots folds =
  match folds with
  | [] -> failwith "no fold to do"
  | Y a :: _ -> (* fold y *)
      S.map (fun (x,y) -> if y < a then (x,y) else (x,(a - (y - a)))) dots
  | X a :: _ -> (* fold x *)
      S.map (fun (x,y) -> if x < a then (x,y) else ((a - (x - a)), y)) dots

let test_input = "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"

let run input =
  let dots,folds = parse input in
  let dots = S.of_list dots in
  do_one_fold dots folds
  |> S.cardinal

let () =
  assert ((run test_input) == 17);
  let input = Helpers.input 13 in
  print_int (run (Lwt_main.run input))
