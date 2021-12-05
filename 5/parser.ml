open Angstrom

type point = {x : int; y : int};;

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

let p_point =
  lift2 (fun a b -> {x = a; y = b})
  (p_digits <* char ',')
  (p_digits)

let p_line =
  lift2 (fun a b -> a,b)
  (p_point <* string " -> ")
  (p_point)

let p_lines =
  sep_by1 p_new_line p_line <* skip_while (fun a -> a = '\n')

let parse input =
  CCResult.get_or_failwith (parse_string ~consume:All p_lines input)
