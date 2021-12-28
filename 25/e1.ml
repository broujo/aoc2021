type cucumber =
  | Empty
  | East
  | South

module C = struct
  type t = Int.t * Int.t
  let compare (x0,y0) (x1,y1) =
    match Int.compare x0 x1 with
    | 0 -> Int.compare y0 y1
    | c -> c
end

module M = CCMap.Make(C)
  
let south_ind (i,j) m =
  match M.get (i+1,j) m with
  | None -> (0,j)
  | Some _ -> (i+1,j)

let east_ind (i,j) m =
  match M.get (i,j+1) m with
  | None -> (i,0)
  | Some _ -> (i,j+1)

let is_empty c =
  match c with
  | Empty -> true
  | _ -> false
 
let make_map m l =
  CCList.foldi
    (fun m i col ->
      CCList.foldi (fun m j e -> M.add (i,j) e m) m col)
    m
    l
      
let move_east m =
  M.fold (fun p _ acc ->
    match M.get p m with
    | None -> failwith "impossible case"
    | Some(Empty) -> acc
    | Some(South) -> acc
    | Some(East) ->
        let p2 = east_ind p m in
        if is_empty (M.find p2 m)
        then
          acc
          |> M.update p (fun _ -> Some(Empty))
          |> M.update p2 (fun _ -> Some(East)) 
        else acc) m m

let move_south m =
  M.fold (fun p _ acc ->
    match M.get p m with
    | None -> failwith "impossible case"
    | Some(Empty) -> acc
    | Some(South) ->
        let p2 = south_ind p m in
        if is_empty (M.find p2 m)
        then
          acc
          |> M.update p (fun _ -> Some(Empty))
          |> M.update p2 (fun _ -> Some(South)) 
        else acc
    | Some(East) -> acc) m m
 
let step m =
  m
  |> move_east
  |> move_south

let solve m =
  let rec aux m n =
    let m2 = step m in
    if m2 = m then n
    else aux m2 (n+1)
  in aux m 1

let test_input =
  "v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>"

let to_cucumber c =
  match c with
  | '.' -> Empty
  | '>' -> East
  | 'v' -> South
  | _ -> failwith "parsing error"

let parse s =
  s
  |> CCString.lines
  |> CCList.map CCString.to_list
  |> CCList.map (CCList.map to_cucumber)
  |> make_map M.empty

let run input =
  let m = parse input in
  solve m

let () =
  assert ((run test_input) = 58);
  let input = Helpers.input 25 in
  print_int (run (Lwt_main.run input))
