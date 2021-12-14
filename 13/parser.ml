open Angstrom

(*    x0 x1 x2 x3 ..
 * y0 .  #  .  .  ..
 * y1 .  #  #  #  ..
 * ..
 *)

type fold = X of int | Y of int

let is_digit =
  function '0' .. '9' -> true | _ -> false

let p_digits =
  lift int_of_string
    (take_while1 is_digit)

let is_space =
  function ' ' -> true | _ -> false

let p_spaces =
  take_while1 is_space
 
let p_new_line =
  char '\n'

let p_dot =
  lift2 (fun a b -> a,b)
  (p_digits <* char ',')
  p_digits

let p_dots =
  sep_by1 p_new_line p_dot

let p_fold_y =
  lift (fun a -> Y(a))
  (string "fold along y=" *> p_digits)

let p_fold_x =
  lift (fun a -> X(a))
  (string "fold along x=" *> p_digits)

let p_fold = p_fold_x <|> p_fold_y

let p_folds =
  sep_by1 p_new_line p_fold

let p_all =
  lift2 (fun a b -> a,b)
  (p_dots <* p_new_line <* p_new_line)
  p_folds

let parse input =
  CCResult.get_or_failwith (parse_string ~consume:Prefix p_all input)
