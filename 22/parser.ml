open Angstrom

type rect =
  | NulRect
  | Rect of ((int*int)*(int*int)*(int*int))

type action =
  | On of rect
  | Off of rect

let to_action on rect =
  if on then On rect
  else Off rect

(* will parse stuff like 42-32 and raise a failure, it's ok *)
let is_digit =
  function '0' .. '9' | '-' -> true | _ -> false

let p_digits =
  lift int_of_string
    (take_while1 is_digit)

let p_range =
  lift2 (fun x y -> x, y)
  (p_digits <* char '.' <* char '.')
  p_digits

  (* x=-20..26,y=-36..17,z=-47..7 *)
let p_rect =
  lift3 (fun x y z -> Rect (x,y,z))
  (string "x="  *> p_range)
  (string ",y=" *> p_range)
  (string ",z=" *> p_range)
  
let p_on =
  lift (fun rect -> On rect)
  (string "on " *> p_rect)

let p_off =
  lift (fun rect -> Off rect)
  (string "off " *> p_rect)

let p_cuboid =
  p_on <|> p_off

let parse input =
  CCResult.get_or_failwith (parse_string ~consume:All p_cuboid input)
