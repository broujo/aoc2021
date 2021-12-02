open Angstrom

type command =
  | Forward of int
  | Down of int

let command_of_string_with_int s v =
  match s with
  | "forward" -> Forward v
  | "up" -> Down (-v)
  | "down" -> Down v
  | _ -> failwith ("Unknown command: " ^ s)

let is_digit =
    function '0' .. '9' -> true | _ -> false

let is_lower =
  function 'a' .. 'z' -> true | _ -> false

let digits =
  lift int_of_string
    (take_while1 is_digit)

let parse_command =
  lift2 command_of_string_with_int
    (take_while1 is_lower <* char ' ')
    digits

let parse entry =
  let r = parse_string ~consume:All parse_command entry
  in CCResult.get_or_failwith r

