module C = struct
  type t = Int.t * Int.t
  let compare (x0,y0) (x1,y1) =
    match Int.compare x0 x1 with
    | 0 -> Int.compare y0 y1
    | c -> c
end

type marked = Lum | NLum (* Has luminated, or has not *)

module M = CCMap.Make(C)
  
let print_map m =
  M.iter (fun k (e,_) ->
    CCFormat.printf "(%d,%d): %d\n" (fst k) (snd k) e;) m

let get_neigh_indices (i,j) =
  let j_l =  CCList.range (j-1) (j+1) in
  let i_l =  CCList.range (i-1) (i+1) in
  CCList.product (fun a b -> (a,b)) i_l j_l
  |> CCList.filter (fun e -> e <> (i,j))

let make_map m l =
  CCList.foldi
    (fun m i col ->
      CCList.foldi (fun m j e -> M.add (i,j) e m) m col)
    m
    l

let unlum (i,m) =
  (i,(M.map (fun v ->
    match v with
    | n,Lum when n > 9 -> 0,NLum
    | n,NLum when n < 10 -> n,NLum
    | _ -> failwith "Error in previous functions - pre cond not respected") m))

let increment_all (i,m) =
  (i,(M.map (fun v ->
    match v with
    | n,l -> (n+1),l)) m)

let increment_pos pos m =
  M.update pos (CCOpt.map (fun (i,x) -> (i+1),x)) m

let luminate_neigh pos v (i,m) =
  match v with
  |n,NLum when n > 9 ->
      get_neigh_indices pos
      |> CCFun.flip (CCList.fold_right increment_pos) m
      |> M.update pos (CCOpt.map (fun (n,_) -> n,Lum))
      |> (fun m -> (i+1),m)
  | _ -> (i,m) 
  
let rec luminate (i,m) =
  let (i2,m2) = M.fold (luminate_neigh) m (i,m) in
  if i <> i2
  then luminate (i2,m2)
  else (i2,m2)

let do_once (i,m) =
  (i,m)
  |> increment_all
  |> luminate
  |> unlum

let rec do_n_times n v =
  if n = 0
  then v
  else do_n_times (n-1) (do_once v)

let rec repeat_until_all n (i,m) =
  let (i2,m2) = do_once (i,m) in
  if i2 = i + (M.cardinal m)
  then n
  else repeat_until_all (n+1) (i2,m2)

let solve m = repeat_until_all 1 (0,m)

let test_input =
  "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"

let run input =
  input
  |> CCString.lines
  |> CCList.map CCString.to_list
  |> CCList.map (CCList.map (fun c -> (CCChar.to_int c) - (CCChar.to_int '0')))
  |> make_map M.empty
  |> M.map (fun v -> v,NLum)
  |> solve

let () =
  assert ((run test_input) = 195);
  let input = Helpers.input 11 in
  print_int (run (Lwt_main.run input))
