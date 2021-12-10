let score_char t =
  match t with
  | '(' -> 1
  | '[' -> 2
  | '{' -> 3
  | '<' -> 4
  | _ -> failwith "bad parsing"

let score =
  CCList.fold_left (fun acc e -> acc * 5 + (score_char e)) 0

let solve l =
  let rec aux l acc =
    match l,acc with
    | [],_ -> Some(score acc)
    | (')' :: _), [] -> None
    | ('}' :: _), [] -> None
    | (']' :: _), [] -> None
    | ('>' :: _), [] -> None
    | (')' :: _), t :: _ when t <> '(' -> None
    | (']' :: _), t :: _ when t <> '[' -> None
    | ('}' :: _), t :: _ when t <> '{' -> None
    | ('>' :: _), t :: _ when t <> '<' -> None
    | (')' :: q), t :: q2 when t = '(' -> aux q q2
    | ('}' :: q), t :: q2 when t = '{' -> aux q q2
    | (']' :: q), t :: q2 when t = '[' -> aux q q2
    | ('>' :: q), t :: q2 when t = '<' -> aux q q2
    | (t :: q), _ -> aux q (t::acc) in
  aux l []
  
let take_middle l =
  let s = CCList.length l in
  if s mod 2 = 0
  then failwith "it's supposed to be odd"
  else CCList.nth l (s/2)

let test_input =
  "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"

let run input =
  input
  |> CCString.lines
  |> CCList.map (CCString.to_list)
  |> CCList.map solve
  |> CCList.filter_map (CCFun.id)
  |> CCList.sort (Int.compare)
  |> take_middle

let () =
  assert (run test_input == 288957);
  let input = Helpers.input 10 in
  print_int (run (Lwt_main.run input))
