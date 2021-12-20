let dosum_until m =
  let rec aux n m acc =
    if acc >= m  then (n-1)
    else aux (n+1) m (n+acc) in
  if m >= 0 then aux 0 m 0
  else -(aux 0 (-m) 0)

let y_born t_y = 
  let y1,y2 = t_y in
  let y = min y1 y2 in
  let max_y = (CCInt.abs y) - 1 in
  let min_y = y in
  min_y, max_y

let x_born t_x =
  let x1,x2 = t_x in
  let max_x = max x1 x2 in
  let mx = min x1 x2 in
  let min_x = dosum_until mx in
  min_x, max_x

let will_touch (t_x_min, t_x_max) (t_y_min, t_y_max) v =
  let t_x_min, t_x_max =
    let a = min t_x_min t_x_max in
    let t_x_max = max t_x_min t_x_max in
    a, t_x_max in

  let t_y_min, t_y_max =
    let a = min t_y_min t_y_max in
    let t_y_max = max t_y_min t_y_max in
    a, t_y_max in

  let rec aux (x,y) (vel_x, vel_y) =
    if x > t_x_max || y < t_y_min then false
    else if x <= t_x_max && x >= t_x_min && y <= t_y_max && y >= t_y_min then true
    else aux
      ((x+vel_x), (y+vel_y))
      ((vel_x + (CCInt.compare 0 vel_x)),(vel_y - 1)) in

  aux (0,0) v

let _input = "target area: x=56..76, y=-162..-134"
let _test_input = "target area: x=20..30, y=-10..-5"

let input_x = (56,76)
let input_y = (-162,-134)

let test_x = (20,30)
let test_y = (-10,-5)

let run (t_x,t_y) =
  let min_y, max_y = y_born t_y in
  let min_x, max_x = x_born t_x in

  let x_s =  CCSeq.range (min_x) (max_x) in
  let y_s =  CCSeq.range (min_y) (max_y) in
  CCSeq.product x_s y_s
  |> CCSeq.filter (will_touch t_x t_y)
  |> CCSeq.length

let () =
  assert ((run (test_x,test_y)) = 112);
  let input = (input_x,input_y) in
  print_int (run input)
