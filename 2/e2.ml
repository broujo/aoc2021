open Parser

let step (h, v, aim) c =
  match c with
  | Forward x -> (h + x), (v + aim * x), aim
  | Down x -> h, v, (aim + x)

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
  |> CCList.fold_left step (0,0,0)
  |> fun (a,b,_) -> a*b


let () =
  assert ((run test_input) == 900);
  let input = Helpers.input 2 in
  print_int (run (Lwt_main.run input))
