open Parser

(* https://stackoverflow.com/questions/3989776/transpose-of-a-list-of-lists *)
let rec transpose l =
  match l with
  | [] -> []
  | [] :: xss -> transpose xss
  | (x::xs) :: xss ->
      (x :: CCList.map CCList.hd xss) :: transpose (xs :: CCList.map CCList.tl xss)

let score remaining_board last =
  let sum =
    CCList.fold_right
      (fun l acc -> CCList.fold_right (+) l acc)
      remaining_board
      0
  in sum * last

let remove_last l last =
  CCList.map (fun e -> CCList.filter (fun a -> a <> last) e) l

let solve_board b draws =
  let row_l = b
  in let column_l = transpose row_l
  in let rec aux rows columns draws acc =
    match draws with
    | [] -> 0,draws
    | t :: q ->
        let r2 = remove_last rows t
        in let c2 = remove_last columns t
        in if CCList.exists (fun l -> l = []) (r2 @ c2)
        then (score r2 t),(CCList.rev (t::acc))
        else aux r2 c2 q (t::acc)
  in aux row_l column_l draws []

let solve boards draws =
  CCList.fold_right
    (fun board (s,dr) ->
      let (s2,dr2) = solve_board board draws
      in if ((CCList.length dr2) > (CCList.length dr)) then (s2,dr2) else (s,dr))
    boards
    (0,[])
        
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
  assert ((run test_input) == 1924);
  let input = Helpers.input 4 in
  print_int (run (Lwt_main.run input))
