let solve l =
  let rec aux l acc =
    match l,acc with
    | [],_ -> 0
    | (')' :: _), [] -> 3
    | ('}' :: _), [] -> 57
    | (']' :: _), [] -> 1197
    | ('>' :: _), [] -> 25137
    | (')' :: _), t :: _ when t <> '(' -> 3
    | (']' :: _), t :: _ when t <> '[' -> 57
    | ('}' :: _), t :: _ when t <> '{' -> 1197
    | ('>' :: _), t :: _ when t <> '<' -> 25137
    | (')' :: q), t :: q2 when t = '(' -> aux q q2
    | ('}' :: q), t :: q2 when t = '{' -> aux q q2
    | (']' :: q), t :: q2 when t = '[' -> aux q q2
    | ('>' :: q), t :: q2 when t = '<' -> aux q q2
    | (t :: q), _ -> aux q (t::acc) in
  aux l []
  
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
  |> CCFun.flip (CCList.fold_right (fun e acc -> e + acc)) 0
   
let () =
  assert (run test_input == 26397);
  let input = Helpers.input 10 in
  print_int (run (Lwt_main.run input))
