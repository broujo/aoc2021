open Parser

type plan =
  | X of int
  | Y of int
  | Z of int

let is_intersect rect1 rect2 =
    match rect1, rect2 with
    | NulRect, _ -> false
    | _, NulRect -> false
    | Rect ((xa1,xa2),(ya1,ya2),(za1,za2)), Rect ((xb1,xb2),(yb1,yb2),(zb1,zb2)) ->
           min xa1 xa2 <= max xb1 xb2
        && min xb1 xb2 <= max xa1 xa2
        && min ya1 ya2 <= max yb1 yb2
        && min yb1 yb2 <= max ya1 ya2
        && min za1 za2 <= max zb1 zb2
        && min zb1 zb2 <= max za1 za2

(* 6 plans définis par x = x1; x = x2; ... *)
let bind f rect =
  match rect with
  | NulRect -> NulRect
  | Rect rect' -> f rect'

let bind_pair f rect =
  match rect with
  | NulRect -> NulRect,NulRect
  | Rect rect' -> f rect'

(* cut X x -> (<x, >=x) *)
let cut_with_plan rect plan =
  bind_pair (fun rect' ->
    let ((x1,x2),(y1,y2),(z1,z2)) = rect' in
    let (x,y,z) = rect' in
    match plan with
    | X x_cut ->
        if x_cut > max x1 x2
        then Rect rect', NulRect
        else if x_cut <= min x1 x2
        then NulRect, Rect rect'
        else Rect ((x1,x_cut-1),y,z), Rect ((x_cut, x2),y,z)
    | Y y_cut ->
        if y_cut > max y1 y2
        then Rect rect', NulRect
        else if y_cut <= min y1 y2
        then NulRect, Rect rect'
        else Rect (x, (y1,y_cut-1),z), Rect (x,(y_cut, y2),z)
    | Z z_cut ->
        if z_cut > max z1 z2
        then Rect rect', NulRect
        else if z_cut <= min z1 z2
        then NulRect, Rect rect'
        else Rect (x, y, (z1,z_cut-1)), Rect (x,y, (z_cut, z2))
  ) rect

let break rect1 rect2 =
  match rect1, rect2 with
  | _,NulRect -> NulRect, []
  | NulRect,_ -> NulRect, [rect2]
  | Rect rect1, _ ->

  let ((x1,x2),(y1,y2),(z1,z2)) = rect1 in
  let l = [] in
  let rect2' = rect2 in

  let rect21,rect2' = cut_with_plan rect2'  (X (min x1 x2)) in
  let l = rect21 :: l in
  let rect2',rect22 = cut_with_plan rect2' (X ((max x1 x2) + 1)) in
  let l = rect22 :: l in
  
  let rect21,rect2' = cut_with_plan rect2'  (Y (min y1 y2)) in
  let l = rect21 :: l in
  let rect2',rect22 = cut_with_plan rect2' (Y ((max y1 y2) + 1)) in
  let l = rect22 :: l in

  let rect21,rect2' = cut_with_plan rect2'  (Z (min z1 z2)) in
  let l = rect21 :: l in
  let rect2',rect22 = cut_with_plan rect2' (Z ((max z1 z2) + 1)) in
  let l = rect22 :: l in
  
  let l = CCList.filter (fun r -> r <> NulRect) l in

  rect2', l

let area r =
  match r with
  | NulRect -> 0
  | Rect ((x1,x2),(y1,y2),(z1,z2)) ->
      let x = CCInt.abs (x1 - x2) + 1 in
      let y = CCInt.abs (y1 - y2) + 1 in
      let z = CCInt.abs (z1 - z2) + 1 in
      x * y * z

let compare rect1 rect2 =
  CCInt.compare (area rect1) (area rect2)

let max rect1 rect2 =
  if compare rect1 rect2 > 0 then rect1
  else rect2

let min rect1 rect2 =
  if compare rect1 rect2 < 0 then rect1
  else rect2

(* must add rect1 after the fact if on *)
let off_one rect1 rect2 =
  let _intersect, break_l = break rect1 rect2 in
  break_l

let on l rect =
  rect :: (CCList.flat_map (off_one rect) l)

let off l rect2 =
  CCList.flat_map (off_one rect2) l

let do_action l a =
  match a with
  | On r -> on l r
  | Off r -> off l r

let intersect rect1 rect2 =
  let intersect, _break_l = break rect1 rect2 in
  intersect

let ignore_out_50 l =
  let c50 = Rect ((-50,50),(-50,50),(-50,50)) in
  CCList.map (intersect c50) l

let test_input_1 =
  "on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10"

let test_input_2 =
  "on x=-20..26,y=-36..17,z=-47..7
on x=-20..33,y=-21..23,z=-26..28
on x=-22..28,y=-29..23,z=-38..16
on x=-46..7,y=-6..46,z=-50..-1
on x=-49..1,y=-3..46,z=-24..28
on x=2..47,y=-22..22,z=-23..27
on x=-27..23,y=-28..26,z=-21..29
on x=-39..5,y=-6..47,z=-3..44
on x=-30..21,y=-8..43,z=-13..34
on x=-22..26,y=-27..20,z=-29..19
off x=-48..-32,y=26..41,z=-47..-37
on x=-12..35,y=6..50,z=-50..-2
off x=-48..-32,y=-32..-16,z=-15..-5
on x=-18..26,y=-33..15,z=-7..46
off x=-40..-22,y=-38..-28,z=23..41
on x=-16..35,y=-41..10,z=-47..6
off x=-32..-23,y=11..30,z=-14..3
on x=-49..-5,y=-3..45,z=-29..18
off x=18..30,y=-20..-8,z=-3..13
on x=-41..9,y=-7..43,z=-33..15
on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
on x=967..23432,y=45373..81175,z=27513..53682"


let run input =
  input
  |> CCString.lines
  |> CCList.map parse
  |> CCList.fold_left do_action []
  |> ignore_out_50
  |> CCList.map area
  |> CCList.fold_left (+) 0

let () =
  assert (run test_input_1 = 39);
  assert (run test_input_2 = 590784);
  let input = Helpers.input 22 in
  print_int (run (Lwt_main.run input))
