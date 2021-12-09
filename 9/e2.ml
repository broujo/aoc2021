module C = struct
  type t = Int.t * Int.t
  let compare (x0,y0) (x1,y1) =
    match Int.compare x0 x1 with
    | 0 -> Int.compare y0 y1
    | c -> c
end

type marked = Marked | Unmarked

module M = CCMap.Make(C)
  
let print_map m =
  M.iter (fun k e ->
    CCFormat.printf "(%d,%d): %d\n" (fst k) (snd k) e;) m

let get_neigh_indices (i,j) =
  let j_l =  CCList.range (j-1) (j+1)
    |> CCList.map (fun j -> (i,j)) in
  let i_l =  CCList.range (i-1) (i+1)
    |> CCList.map (fun i -> (i,j)) in
  j_l @ i_l
  |> CCList.filter (fun e -> e <> (i,j))

let check_neigh m p =
  let elem = M.find p m in
  get_neigh_indices p
  |> CCList.for_all (fun p ->
      match M.get p m with
      | None -> true
      | Some(v) -> elem < v)

let bassin_size low m =
  let rec bassins low (m,acc) =
    match M.get low m with
    | None -> m,acc
    | Some(_,Marked) -> m,acc
    | Some(9,_) -> m,acc
    | Some(i,_) -> 
        let m = M.update low (function | None -> None | _ -> Some(i,Marked)) m in
        get_neigh_indices low
        |> CCFun.flip (CCList.fold_right (fun p (m,acc) -> bassins p (m,acc))) (m,low::acc) in
  let map,l = bassins low (m,[]) in map,(CCList.length l)

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
  let map =
    input
    |> CCString.lines
    |> CCList.map CCString.to_list
    |> CCList.map (CCList.map (fun c -> (CCChar.to_int c) - (CCChar.to_int '0')))
    |> make_map M.empty in
  map
  |> M.filter (fun p _ -> check_neigh map p)
  |> M.to_list
  |> CCList.fold_filter_map (fun map (low,_) ->
      let map,size = bassin_size low map in
      if size = 0
      then map,None
      else map,Some(size)) (M.map (fun e -> e,Unmarked) map)
  |> snd
  |> CCList.sort (fun a b -> CCInt.compare b a)
  |> (fun l ->
      match l with
      | [] | _ :: [] | _ :: _  :: [] -> failwith "error less than 3 bassins"
      | a :: b :: c :: _ -> a * b * c)
     

let () =
  assert ((run test_input) == 1134);
  let input = Helpers.input 9 in
  print_int (run (Lwt_main.run input))
