type amphipod =
  | Amber
  | Bronze
  | Copper
  | Desert

type pos =
  | Room of (amphipod * int)
  | Hallway of int

let compare_pos p1 p2 =
  match p1, p2 with
  | Room _, Hallway _ -> 1
  | Hallway _, Room _ -> -1
  | Room _, Room _ -> Stdlib.compare p1 p2
  | Hallway m, Hallway n ->
      let r = CCInt.compare (CCInt.abs (5 - n)) (CCInt.abs (5 - m)) in
      if r = 0 then CCInt.compare n m else r

let is_hallway = function | Hallway _ -> true | _ -> false
let is_room = function | Room _ -> true | _ -> false

let all_amphi = [Amber; Bronze; Copper; Desert]

let all_pos =
  (CCList.product (fun a b -> Room(a, b))
    all_amphi
    [0;1;2;3]) @
  (CCList.map (fun x -> Hallway x) (CCList.range 0 10))

let sides = [ Hallway 0; Hallway 1; Hallway 9; Hallway 10]
let mid = [ Hallway 4; Hallway 6; Hallway 8]
let rooms = (CCList.product (fun a b -> Room(a, b)) all_amphi [0;1;2;3])

let rec dist p1 p2 =
  match p1,p2 with
  | Hallway i, Hallway j -> CCInt.abs (j - i)
  | Room (Amber , n), p | p, Room (Amber , n) -> n + 1 + (dist (Hallway 2) p)
  | Room (Bronze, n), p | p, Room (Bronze, n) -> n + 1 + (dist (Hallway 4) p)
  | Room (Copper, n), p | p, Room (Copper, n) -> n + 1 + (dist (Hallway 6) p)
  | Room (Desert, n), p | p, Room (Desert, n) -> n + 1 + (dist (Hallway 8) p)

let score_a p1 p2 a =
  let multiplier =
    match a with
    | Amber -> 1
    | Bronze -> 10
    | Copper -> 100
    | Desert -> 1000 in
  multiplier * (dist p1 p2)

module Pos = struct
  type t = pos
  let compare = compare_pos
end

module M = CCMap.Make(Pos)
module S = CCSet.Make(Pos)

let to_s_a a =
  match a with
  | Amber -> "A"
  | Bronze -> "B"
  | Copper -> "C"
  | Desert -> "D"

let to_s_pos p =
  match p with
  | Room (a, n) -> "Room (" ^ (to_s_a a) ^ "," ^ (string_of_int n) ^ ")"
  | Hallway n -> "Hall (" ^ (string_of_int n) ^ ")"

let neigh p =
  match p with
  | Hallway 0 -> [Hallway 1]
  | Hallway 1 -> [Hallway 0; Hallway 3; Room (Amber, 0) ]
  | Hallway 3 -> [Hallway 1; Room (Amber, 0); Room (Bronze, 0); Hallway 5]
  | Hallway 5 -> [Room (Bronze, 0); Hallway 3; Hallway 7; Room (Copper, 0)]
  | Hallway 7 -> [Room (Desert, 0); Hallway 5; Hallway 9; Room (Copper, 0)]
  | Hallway 9 -> [Room (Desert, 0); Hallway 7; Hallway 10]
  | Hallway 10 -> [Hallway 9]
  | Room (Amber, 0) -> [Hallway 1; Hallway 3; Room (Amber, 1)]
  | Room (Amber, 1) -> [Room (Amber, 0); Room (Amber, 2)]
  | Room (Amber, 2) -> [Room (Amber, 1); Room (Amber, 3)]
  | Room (Amber, 3) -> [Room (Amber, 2)]
  | Room (Bronze, 0) -> [Hallway 3; Hallway 5; Room (Bronze, 1)]
  | Room (Bronze, 1) -> [Room (Bronze, 0); Room (Bronze, 2)]
  | Room (Bronze, 2) -> [Room (Bronze, 1); Room (Bronze, 3)]
  | Room (Bronze, 3) -> [Room (Bronze, 2)]
  | Room (Copper, 0) -> [Hallway 5; Hallway 7; Room (Copper, 1)]
  | Room (Copper, 1) -> [Room (Copper, 0); Room (Copper, 2)]
  | Room (Copper, 2) -> [Room (Copper, 1); Room (Copper, 3)]
  | Room (Copper, 3) -> [Room (Copper, 2)]
  | Room (Desert, 0) -> [Hallway 7; Hallway 9; Room (Desert, 1)]
  | Room (Desert, 1) -> [Room (Desert, 0); Room (Desert, 2)]
  | Room (Desert, 2) -> [Room (Desert, 1); Room (Desert, 3)]
  | Room (Desert, 3) -> [Room (Desert, 2)]
  | _ -> failwith ((to_s_pos p) ^ "should never be used")

let right_pos m p =
  let a = M.get p m in
  match a, p with
  | Some a, Room (b, n) ->
      a = b &&
      (CCList.range_by ~step:1 (n+1) 3
      |> CCList.map (fun i -> Room (b, i))
      |> CCList.for_all (fun p ->
          match M.get p m with
                   | None -> false
                   | Some a -> a = b))
  | _ -> false

let adjacents p m checked =
  let rec aux p (emptys,adjacents) =
    if S.mem p emptys || S.mem p adjacents
    then emptys, adjacents
    else if M.mem p m
    then emptys, (S.add p adjacents)
    else CCList.fold_right aux (neigh p) ((S.add p emptys), adjacents)
  in
  neigh p
  |> CCList.filter (fun n -> not (S.mem n checked))
  |> CCList.fold_left (fun l n ->
      if CCList.exists (fun (e, a) -> S.mem n e || S.mem n a) l
      then l
      else (aux n (S.empty,(S.add p S.empty))) :: l) []

let accessible m =
  M.fold (fun p _ (checked, adj) ->
    let l = adjacents p m checked in
    CCList.fold_left (fun (checked, adj) (e, a) ->
      let checked = S.union a (S.union e checked) in
      checked,(S.fold (fun p adj ->
        M.update p (function
          | None -> Some e
          | Some s -> Some (S.union s e)) adj
        ) a adj)
      ) (checked,adj) l
    ) m (S.empty, M.empty)

let possible_moves m =
  let ok_rooms = CCList.filter (fun a ->
    CCList.(0 -- 3)
    |> CCList.map (fun i -> Room (a, i))
    |> CCList.for_all (fun p ->
        match M.get p m with
        | None -> true
        | Some b -> a = b)) all_amphi in
  accessible m
  |> snd
  |> M.mapi (fun p s ->
    match p, M.find p m with
    | Hallway _, a -> S.filter (function
      | Room (b, _) -> a = b && CCList.mem a ok_rooms
      | _ -> false) s
    | Room (c, _), a -> S.filter (function
      | Room (b, _) -> a = b && b <> c && CCList.mem a ok_rooms
      | Hallway _ -> not (CCList.mem c ok_rooms)) s)
  |> M.map (fun s ->
      if not (S.exists is_room s)
      then s
      else
        let s = S.filter is_room s in
        let f p max = match p, max with
          | Room (_, n), Some(Room (_, m)) when n > m -> Some p
          | Room _, None -> Some p
          | _ -> max in

        S.fold f s None 
        |> CCOption.map_or ~default:S.empty S.singleton)

let order_try =
  CCList.flat_map (fun b ->
    CCList.range (0) 3
    |> CCList.map (fun i -> Room (b, i))) [Bronze; Desert; Copper; Amber]
  |> CCList.rev

let move p p2 a m =
  let m =
    M.update p (fun _ -> None) m
    |> M.update p2 (fun _ -> Some a) in
  m,(score_a p p2 a)

module CacheMap = struct
  type t = amphipod M.t
  let compare = M.compare (Stdlib.compare)
end
module Cache = CCMap.Make(CacheMap)

module MSetS = struct
  type t = (amphipod M.t) * int
  let compare (m1, s1) (m2, s2) =
    match CCInt.compare s1 s2 with
    | 0 -> M.compare (Stdlib.compare) m1 m2
    | r -> r
end
module MSet = CCSet.Make(MSetS)

let rec bf_tail_rec m_set cache best =
  if MSet.is_empty m_set then best else

  let m,score = MSet.choose m_set in
  let m_set = MSet.remove (m,score) m_set in

  if Cache.mem m cache && Cache.find m cache <= score then bf_tail_rec m_set cache best else
  let cache = Cache.update m (fun _ -> Some score) cache in

  if score >= best then bf_tail_rec m_set cache best else
  if (M.for_all (fun p _ -> right_pos m p) m) then bf_tail_rec m_set cache score else

  let possibles = possible_moves m in

  let m1, _ = M.partition (fun p _ -> S.exists is_room (M.find p possibles)) m in
  match M.choose_opt m1 with
  | Some (p,a) ->
      let s = M.find p possibles in
      (* assert(S.cardinal s = 1); *)
      let p2 = S.choose s in
      let m,d  = move p p2 a m in
      let score' = score + d in
      if score' >= best then bf_tail_rec m_set cache score
      else
        let m_set = MSet.add (m, score') m_set in
        bf_tail_rec m_set cache best

  | None ->
      let f p = match M.get p m with
        | None -> None
        | Some a ->
            if right_pos m p then None else
            let s = M.find p possibles in
            if S.is_empty s then None
              else
                let l = S.to_list s in
                let f' p2 =
                  let m, d = move p p2 a m in
                  let score' = score + d in
                  if score' >= best then None
                  else Some (m, score') in
                Some (CCList.filter_map f' l) in 
      let next_tests = CCList.flatten @@ CCList.filter_map f order_try in
      let m_set = CCList.fold_right MSet.add next_tests m_set in
      bf_tail_rec m_set cache best

let bf m = bf_tail_rec (MSet.singleton (m, 0)) Cache.empty CCInt.max_int
  
let lvl1 =
  all_amphi
  |> CCList.flat_map (fun a -> CCList.map (fun i -> Room (a, i), a) CCList.(2--3))
  |> M.of_list

let input =
  lvl1
  |> M.add (Room (Amber, 0)) Amber
  |> M.add (Room (Amber, 1)) Desert
  |> M.add (Room (Bronze, 0)) Copper
  |> M.add (Room (Bronze, 1)) Amber
  |> M.add (Room (Copper, 0)) Bronze
  |> M.add (Room (Copper, 1)) Desert
  |> M.add (Room (Desert, 0)) Copper
  |> M.add (Room (Desert, 1)) Bronze

let _ = print_int @@ bf input

(* let empty_pos = CCList.filter (fun x -> M.mem x m) (sides @ mid @ rooms) in *)
  


