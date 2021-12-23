module C = struct
  type t = CCInt.t * CCInt.t
  let compare (x0,y0) (x1,y1) =
    match CCInt.compare x0 x1 with
    | 0 -> CCInt.compare y0 y1
    | c -> c
end

module SC = CCSet.Make(C)
module S = CCSet.Make(CCInt)
  
let get_neigh_indices (i,j) =
  let j_l =  CCList.range (j-1) (j+1) in
  let i_l =  CCList.range (i-1) (i+1) in
  CCList.product (fun a b -> (a,b)) i_l j_l

let make_set l =
  CCList.foldi
    (fun s i col ->
      CCList.foldi (fun s j c -> match c with |'#' -> SC.add (i,j) s | _ -> s) s col)
    SC.empty
    l

let to_i l =
  CCList.fold_left (fun acc e -> 2*acc + e) 0 l

let odd_to_i lighted p =
  get_neigh_indices p
  |> CCList.map (fun p -> if SC.mem p lighted then 1 else 0)
  |> to_i

let even_to_i darked p =
  get_neigh_indices p
  |> CCList.map (fun p -> if SC.mem p darked then 0 else 1)
  |> to_i

let odd lighted algorithm =
  SC.to_list lighted
  |> CCList.flat_map get_neigh_indices
  |> CCList.sort_uniq ~cmp:C.compare
  |> CCList.map (fun p -> p,(odd_to_i lighted p))
  |> CCList.filter_map (fun (p, n) -> if S.mem n algorithm then None else Some p) (* darked list *)
  |> SC.of_list

let even darked algorithm =
  SC.to_list darked
  |> CCList.flat_map get_neigh_indices
  |> CCList.sort_uniq ~cmp:C.compare
  |> CCList.map (fun p -> p,(even_to_i darked p))
  |> CCList.filter_map (fun (p, n) -> if S.mem n algorithm then Some p else None) (* darked list *)
  |> SC.of_list

let rec repeat n image algorithm =
  if n = 0 then image
  else if n mod 2 = 0 then repeat (n-1) (odd image algorithm) algorithm
  else repeat (n-1) (even image algorithm) algorithm

let test_input =
  "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###"

let run input =
  let algorithm, image =
    match CCString.split ~by:"\n\n" input with
    | a :: q :: [] -> a, q
    | _ -> failwith "bad parsing" in

  let algorithm =
    algorithm
    |> CCString.to_list
    |> CCList.foldi (fun s i e -> match e with | '#' -> S.add i s | _ -> s) S.empty in

  let image =
    image
    |> CCString.lines
    |> CCList.map CCString.to_list
    |> make_set in

  repeat 50 image algorithm
  |> SC.cardinal


let () =
  let input = Helpers.input 20 in
  print_int (run (Lwt_main.run input))
