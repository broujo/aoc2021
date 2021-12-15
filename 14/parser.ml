open Angstrom

module C = struct
  type t = CCChar.t * CCChar.t
  let compare (x0,y0) (x1,y1) =
    match CCChar.compare x0 x1 with
    | 0 -> CCChar.compare y0 y1
    | c -> c
end

module M = CCMap.Make(C)

let is_digit =
  function '0' .. '9' -> true | _ -> false

let p_digits =
  take_while1 is_digit

let p_new_line =
  char '\n'

let p_input =
  take_till (fun x -> x = '\n')

let p_trans_left =
  lift2 (fun c1 c2 -> c1,c2)
  any_char
  any_char

let p_transform =
  lift2 (fun l r -> l,r)
  (p_trans_left <* string " -> ")
  any_char

let p_transforms =
  lift (fun l -> CCList.fold_right (fun (l,r) acc -> M.add l r acc) l M.empty)
  (sep_by1 p_new_line p_transform)

let p_all =
  lift2 (fun a b -> ((CCString.to_list a),b))
  (p_input <* p_new_line <* p_new_line)
  p_transforms

let parse r =
  CCResult.get_or_failwith (parse_string ~consume:Prefix p_all r)
