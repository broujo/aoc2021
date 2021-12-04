open Parser

(* https://stackoverflow.com/questions/3989776/transpose-of-a-list-of-lists *)
let rec transpose l =
  match l with
  | [] -> []
  | [] :: xss -> transpose xss
  | (x::xs) :: xss ->
      (x :: CCList.map CCList.hd xss) :: transpose (xs :: CCList.map CCList.tl xss)

let solve_board b draws =
  let row_l = b
  in let column_l = transpose row_l
  in let rec aux rows columns draws acc =
    match draws with
    | [] -> 0,draws
    | t :: q ->
        let r2 = CCList.map (fun row -> CCList.filter (fun a -> a <> t) row) rows
        in let c2 = CCList.map (fun col -> CCList.filter (fun a -> a <> t) col) columns
        in if CCList.exists (fun l -> l = []) (r2 @ c2)
        then ((CCList.fold_right (fun l acc -> CCList.fold_right (+) l acc) r2 0)*t),(CCList.rev (t::acc))
        else aux r2 c2 q (t::acc)
  in aux row_l column_l draws []

let solve boards draws =
  CCList.fold_right
    (fun board (s,dr) ->
      let (s2,dr2) = solve_board board dr
      in if s2 <> 0 then (s2,dr2) else (s,dr))
    boards
    (0,draws)

        
let test_input =
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"

let run input =
  let draws,boards = input
  |> parse
  in let (s,_) = solve boards draws
  in s
  

let () =
  assert ((run test_input) == 4512);
  let input = Helpers.input 4 in
  print_int (run (Lwt_main.run input))
