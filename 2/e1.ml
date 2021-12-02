open Parser

let step c (h, v) =
  match c with
  | Forward x -> (h + x), v
  | Down x -> h, (v + x)

let test_input = "forward 5
down 5
forward 8
up 3
down 8
forward 2"

let run input =
  input
  |> CCString.lines
  |> CCList.map parse
  |> fun l ->  CCList.fold_right step l (0,0)
  |> fun (a,b) -> a*b


let () =
  assert ((run test_input) == 150);
  let input = Helpers.input 2 in
  print_int (run (Lwt_main.run input))
