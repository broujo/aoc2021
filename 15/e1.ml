module C = struct
  type t = Int.t * Int.t
  let compare (x0,y0) (x1,y1) =
    match Int.compare x0 x1 with
    | 0 -> Int.compare y0 y1
    | c -> c
end

module M = CCMap.Make(C)
  
let get_neigh_indices (i,j) =
  let j_l =  CCList.range (j-1) (j+1) in
  let i_l =  CCList.range (i-1) (i+1) in
  CCList.product (fun a b -> (a,b)) i_l j_l
  |> CCList.filter (fun e -> e <> (i,j))

let get_neigh_indices (i,j) =
  let j_l =  CCList.range (j-1) (j+1)
    |> CCList.map (fun j -> (i,j)) in
  let i_l =  CCList.range (i-1) (i+1)
    |> CCList.map (fun i -> (i,j)) in
  j_l @ i_l
  |> CCList.filter (fun e -> e <> (i,j))

let make_map m l =
  CCList.foldi
    (fun m i col ->
      CCList.foldi (fun m j e -> M.add (i,j) e m) m col)
    m
    l

let min_possible_dist (x0,y0) (x1,y1) =
  CCInt.abs(x1 - x0) + CCInt.abs(y1 - y0)

type possible_nodes = {p:C.t; weight:int; min_res:int; path:C.t list} (* should add path maybe fo debug *)

let cmp_pnodes p1 p2 =
  match CCInt.compare (p1.weight + p1.min_res) (p2.weight + p2.min_res) with
  | 0 -> CCInt.compare (p1.weight) (p2.weight)
  | i -> i

let insert_in_pnodes_list p l =
  match CCList.find_opt (fun p2 -> p2.p = p.p) l with
  | Some(p2) when cmp_pnodes p p2 >= 0 -> l
  | Some(_) ->
      l
      |> CCList.filter (fun p2 -> p2.p <> p.p)
      |> CCList.sorted_insert ~cmp:cmp_pnodes p
  | None -> CCList.sorted_insert ~cmp:cmp_pnodes p l


let insert_list_in_list sorted_l new_elements =
  CCList.fold_right insert_in_pnodes_list new_elements sorted_l (* high chance of messing it up here *)
 
let choose_mindend e1 e2 =
  match e1 with
  | None -> Some e2
  | Some e1 ->
    if e1.weight < e2.weight
    then Some e1
    else Some e2

let get_neigh_possible endd p m =
  get_neigh_indices p.p
  |> CCList.filter_map (fun p2 ->
      CCOption.map (fun v ->
        {p=p2; weight=p.weight + v; min_res=min_possible_dist p2 endd; path=p2 :: p.path})
      (M.get p2 m))

let filter_possibles min_found possibles =
  match min_found with
  | None -> possibles
  | Some min -> CCList.filter (fun p -> p.weight + p.min_res < min.weight) possibles

let rec find_path endd m min_found possibles =
  match possibles with
  | [] -> min_found
  | p :: q ->
      let min_found =
        if p.p = endd
        then choose_mindend min_found p
        else min_found in
      insert_list_in_list q (get_neigh_possible endd p m)
      |> filter_possibles min_found 
      |> find_path endd m min_found
      

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
  find_path endd m None [{p=start; weight=0; min_res=(min_possible_dist start endd); path=[start]}]
  |> CCOption.get_exn_or "Nothing found :("
  |> (fun x -> x.weight)


let () =
  assert ((run test_input) = 40);
  let input = Helpers.input 15 in
  print_int (run (Lwt_main.run input))
