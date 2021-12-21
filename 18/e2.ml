open Parser

let pair s1 s2 =
  Pair(s1, s2)

type explote_state =
  | NoExplode
  | Exploded
  | ExpR of int
  | ExpL of int
  | ExpLR of (int * int)

let rec snail_to_string snail =
  match snail with
  | Number x -> string_of_int x
  | Pair (snail1, snail2) ->
      let s1 = snail_to_string snail1 in
      let s2 = snail_to_string snail2 in
      "[" ^ s1 ^ "," ^ s2 ^ "]"

let explode s =
  let rec aux snail state depth =
    match snail,state with
    | Number x, ExpR n
    | Number x, ExpL n -> Number (x + n), Exploded
    | Number x, NoExplode -> Number x, state
    | Number _, _ -> failwith "this should never happen"
    | Pair (left, right), ExpR _ ->
        let left, state = aux left state (depth + 1) in
        (pair left right), state
    | Pair (left, right), ExpL _ ->
        let right, state = aux right state (depth + 1) in
        (pair left right), state
    | Pair (Number x, Number y), NoExplode ->
        if depth >= 4
        then Number 0, ExpLR (x, y)
        else snail, state
    | Pair (left, right), NoExplode ->
        let left, state = aux left state (depth + 1) in
        begin match state with
        | Exploded | ExpL _ -> (pair left right), state
        | ExpR _ -> let right, state = aux right state (depth + 1) in (pair left right), state
        | ExpLR (x, y) -> let right, _ = aux right (ExpR y) (depth + 1) in (pair left right), ExpL x
        | NoExplode ->
            let right, state = aux right state (depth + 1) in
            begin match state with
            | Exploded | ExpR _ | NoExplode -> (pair left right), state
            | ExpL _ -> let left, state = aux left state (depth + 1) in (pair left right), state
            | ExpLR (x, y) -> let left, _ = aux left (ExpL x) (depth + 1) in (pair left right), ExpR y
            end
        end
    | _ -> failwith "impossible case" in
  aux s NoExplode 0

let rec split s =
  match s with
  | Number x when x >= 10 -> let left = x / 2 in (pair (Number left) (Number (x - left))), true
  | Number _ -> (s, false)
  | Pair (left, right) ->
      let left,b = split left in
      if b then (pair left right), b
      else let right,b = split right in
      (pair left right),b

let reduce_once snail =
  let snail, state = explode snail in
  match state with
  | NoExplode -> split snail
  | _ -> snail,true

let rec reduce snail =
  let snail, b = reduce_once snail in
  if b then reduce snail
  else snail

let add snail1 snail2 =
  reduce (pair snail1 snail2)

let rec magnitude snail =
  match snail with
  | Number x -> x
  | Pair(left, right) ->
      let ml = magnitude left in
      let rl = magnitude right in
      3*ml + 2*rl

let test_input =
  "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"


let run input =
  input
  |> CCString.lines
  |> CCList.map parse
  |> CCList.diagonal
  |> CCList.flat_map (fun (a,b) -> [(a,b); (b,a)])
  |> CCList.map (fun (a,b) -> add a b)
  |> CCList.map magnitude
  |> CCList.fold_left max CCInt.min_int

let () =
  assert (run test_input = 3993);
  let input = Helpers.input 18 in
  print_int (run (Lwt_main.run input))
