open Parser

module C = struct
  type t = Int.t * Int.t
  let compare (x0,y0) (x1,y1) =
    match Int.compare x0 x1 with
    | 0 -> Int.compare y0 y1
    | c -> c
end

module S = CCSet.Make(C)

let print_set s =
  let min_x = S.fold (fun (e,_) acc -> if e < acc then e else acc) s CCInt.max_int in
  let min_y = S.fold (fun (_,e) acc -> if e < acc then e else acc) s CCInt.max_int in
  let max_x = S.fold (fun (e,_) acc -> if e > acc then e else acc) s CCInt.min_int in
  let max_y = S.fold (fun (_,e) acc -> if e > acc then e else acc) s CCInt.min_int in

  let x_l =  CCList.range min_x max_x in
  let y_l =  CCList.range min_y max_y in
  CCList.iter (fun yi ->
    CCList.iter (fun xi ->
      if S.mem (xi,yi) s then print_string "█" else print_string " ") x_l; print_char '\n') y_l;
  print_char '\n'

let do_one_fold dots fold =
  match fold with
  | Y a -> (* fold y *)
      S.map (fun (x,y) -> if y < a then (x,y) else (x,(a - (y - a)))) dots
  | X a  -> (* fold x *)
      S.map (fun (x,y) -> if x < a then (x,y) else ((a - (x - a)), y)) dots

let do_folds dots folds = 
  CCList.fold_left (fun acc elem -> do_one_fold acc elem) dots folds

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
  do_folds dots folds
  |> print_set

let () =
  run test_input;
  let input = Helpers.input 13 in
  run (Lwt_main.run input)
