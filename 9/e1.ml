module C = struct
  type t = Int.t * Int.t
  let compare (x0,y0) (x1,y1) =
    match Int.compare x0 x1 with
    | 0 -> Int.compare y0 y1
    | c -> c
end

module M = CCMap.Make(C)
  
let print_map m =
  M.iter (fun k e ->
    CCFormat.printf "(%d,%d): %d\n" (fst k) (snd k) e;) m

let check_neigh m (i,j) =
  let elem = M.find (i,j) m in
  let j_l =  CCList.range (j-1) (j+1)
    |> CCList.map (fun j -> (i,j)) in
  let i_l =  CCList.range (i-1) (i+1)
    |> CCList.map (fun i -> (i,j)) in
  j_l @ i_l
  |> CCList.filter (fun e -> e <> (i,j))
  |> CCList.for_all (fun p ->
      match M.get p m with
      | None -> true
      | Some(v) -> elem < v)

let make_map m l =
  CCList.foldi
    (fun m i col ->
      CCList.foldi (fun m j e -> M.add (i,j) e m) m col)
    m
    l

let test_input =
"2199943210
3987894921
9856789892
8767896789
9899965678"

let run input =
    input
    |> CCString.lines
    |> CCList.map CCString.to_list
    |> CCList.map (CCList.map (fun c -> (CCChar.to_int c) - (CCChar.to_int '0')))
    |> make_map M.empty
    |> (fun map -> M.fold (fun p e acc -> if check_neigh map p then e+1+acc else acc) map 0)
   (* |> (fun map -> print_map map; M.fold (fun b e acc -> if check_neigh map p then e+1+acc else acc) map 0) *)


let () =
 (* assert ((run test_input) == 150); *)
  let input = Helpers.input 9 in
  print_int (run (Lwt_main.run input))
