open Angstrom

type snailfish =
  | Number of int
  | Pair of snailfish * snailfish


let is_digit =
  function '0' .. '9' -> true | _ -> false

let p_digits =
  lift int_of_string
    (take_while1 is_digit)

let p_number =
  lift (fun x -> Number x)
  p_digits

let p_snailfish =
  fix (fun snailfish ->
    let p_pair =
      lift2 (fun s1 s2 -> Pair(s1, s2))
      (char '[' *> snailfish)
      (char ',' *> snailfish <* char ']') in
    p_number <|> p_pair)

let parse input =
  CCResult.get_or_failwith (parse_string ~consume:Prefix p_snailfish input)
