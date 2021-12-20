module C = struct
  type t = CCInt.t * CCInt.t
  let compare (x0,y0) (x1,y1) =
    match CCInt.compare x0 x1 with
    | 0 -> CCInt.compare y0 y1
    | c -> c
end

module CS = struct
  type t = CCInt.t * C.t
  let compare (w0,p0) (w1,p1) =
    match CCInt.compare w0 w1 with
    | 0 -> C.compare p0 p1
    | c -> c
end

module M = CCMap.Make(C)
module S = CCSet.Make(CS)

let make_map m l =
  let i_size = CCList.length l in
  let j_size = CCList.length (CCList.hd l) in
  CCList.foldi
    (fun m i col ->
      CCList.foldi ( fun m j e ->
        CCList.fold_left ( fun m five_i ->
          CCList.fold_left ( fun m five_j ->
             M.add (five_i*i_size+i,five_j*j_size+j) (((e + five_i + five_j) - 1) mod 9 + 1) m)
          m (CCList.range 0 4))
        m (CCList.range 0 4))
      m col)
    m l

let get_neigh_indices (i,j) =
  let j_l =  CCList.range (j-1) (j+1)
    |> CCList.map (fun j -> (i,j)) in
  let i_l =  CCList.range (i-1) (i+1)
    |> CCList.map (fun i -> (i,j)) in
  j_l @ i_l
  |> CCList.filter (fun e -> e <> (i,j))

let get_neigh p fmap =
  get_neigh_indices p
  |> CCList.filter_map (fun p_neigh ->
      CCOption.map (fun v -> (v,p_neigh)) (M.get p_neigh fmap))

let update_min_risk p risk min_risk =
  M.update p (function | _ -> Some risk) min_risk

let find_path fmap start =

  (* Map of minimum risk path *)
  let min_risk = update_min_risk start 0 M.empty in

  (* The recursive function that executes until the set of possible path is empty *)
  let rec aux (min_risk, set) =
    if S.is_empty set then (min_risk, set)
    else
      (* get the minimum element from the set
       * remove it
       * get the neighbors
       * and update all the neighbors min_risk *)
      let risk,p = S.min_elt set in
      let set = S.remove (risk, p) set in
      let neighs = get_neigh p fmap in

      let f (risk_p2, p2) (min_risk, set) =
        let risk2 = risk + risk_p2 in
        if risk2 >= (M.get_or p2 min_risk ~default:CCInt.max_int) then
          (min_risk, set)
        else 
          let set = S.remove ((M.get_or p2 min_risk ~default:CCInt.max_int), p2) set in
          let min_risk = update_min_risk p2 risk2 min_risk in
          let set = S.add ((M.find p2 min_risk), p2) set in
          (min_risk, set) 
      in
      aux (List.fold_right f neighs (min_risk, set))
  in
  let set = S.add ((M.find start min_risk), start) S.empty in
  let min_risk, _ = aux (min_risk, set) in
  min_risk



let test_input =
  "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"

let run input =
  let m = input
  |> CCString.lines
  |> CCList.map CCString.to_list
  |> CCList.map (CCList.map (fun c -> (CCChar.to_int c) - (CCChar.to_int '0')))
  |> make_map M.empty in
  
  let endd = M.fold (fun (x,y) _ (mx, my) -> ((if x > mx then x else mx), (if y > my then y else my))) m (CCInt.min_int,CCInt.min_int) in
  let start = (0,0) in
  find_path m start
  |> M.find endd
  |> CCFun.tap (fun i -> print_int i; print_string "\n")


let _:int =
  assert ((run test_input) = 315);
  let input = Helpers.input 15 in
  run (Lwt_main.run input)
