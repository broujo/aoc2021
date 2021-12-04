open Angstrom

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

let p_draws =
  (sep_by1 (char ',') p_digits) <* p_new_line <* p_new_line

let p_board_line =
  skip_while is_space *> sep_by1 p_spaces p_digits

let p_board =
  sep_by1 p_new_line p_board_line

let p_boards =
  sep_by1 (count 2 p_new_line) p_board

let p_all =
  lift2 (fun a b -> a,b)
  p_draws
  p_boards

let parse input =
  CCResult.get_or_failwith (parse_string ~consume:Prefix p_all input)
