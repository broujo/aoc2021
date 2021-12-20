let _input = "target area: x=56..76, y=-162..-134"

let _t_x = (56,76)
let t_y = (-162,-134)

let dosum n =
  let rec aux n acc =
    if n = 0 then acc
    else aux (n-1) (acc+n) in
  aux n 0

let run () =
  let y1,y2 = t_y in
  let y = min y1 y2 in
  dosum ((CCInt.abs y) - 1)

let () =
  print_int (run ())
