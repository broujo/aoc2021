open Angstrom

type op =
  | NOP of int | ACC of int | JMP of int

let op_to_s o =
  match o with
  | NOP n -> Printf.sprintf "%s %d" "nop" n
  | ACC n -> Printf.sprintf "%s %d" "acc" n
  | JMP n -> Printf.sprintf "%s %d" "jmp" n


let pair_to_op (o,n) =
  match o with
  | "nop" -> NOP n
  | "acc" -> ACC n
  | "jmp" -> JMP n
  | _ -> failwith "unknown"

let is_digit =
  function '0' .. '9' -> true | _ -> false

let p_digits =
  take_while1 is_digit

let p_integer =
  lift2 (^)
    ((string "+") <|> (string "-") <|> (return ""))
    p_digits
  >>| int_of_string

let p_instruction =
  lift2 (fun a b -> Some(pair_to_op (a,b)))
    ((take 3) <* (char ' '))
    p_integer

let ignore_empty_line =
  ((char '\n') <|> (return '\n'))
  >>| fun _ -> None

let p_line =
  p_instruction <|> ignore_empty_line

let p_program =
  sep_by1 end_of_line p_line
  >>| CCList.filter_map CCFun.id

let parse_program r =
  CCResult.get_or_failwith (parse_string ~consume:All p_program r)
