(* rules:
  * associate to each segments the possible letters
  * Example :
    * if i see 3 char
    * => seg a = seg c = seg f = fun seg -> seg & [those 3] 
    *    for all other segs : fun seg -> seg - [those 3]
    * if i see 2 chars
    * => seg c = seg 
    *)

module M = CCMap.Make(CCChar)
module S = CCSet.Make(CCChar)

let all_s = S.of_list (CCString.to_list "abcdefg")

let s_zero = S.of_list (CCString.to_list "abcefg")
let s_one = S.of_list (CCString.to_list "cf")
let s_two = S.of_list (CCString.to_list "acdeg")
let s_three = S.of_list (CCString.to_list "acdfg")
let s_four = S.of_list (CCString.to_list "bcdf")
let s_five = S.of_list (CCString.to_list "abdfg")
let s_six = S.of_list (CCString.to_list "abdefg")
let s_seven = S.of_list (CCString.to_list "acf")
let s_eight = S.of_list (CCString.to_list "abcdefg")
let s_nine = S.of_list (CCString.to_list "abcdfg")

let s_digits = [s_zero; s_one; s_two; s_three; s_four; s_five; s_six; s_seven; s_eight; s_nine]

let print_map m =
  M.iter (fun k e ->
    CCFormat.printf "%c: %s\n" k (CCString.of_list (S.to_list e));) m

let find_key_for s m =
  M.to_list m
  |> CCList.map (fun (a,b) -> (S.choose b,a))
  |> M.of_list
  |> M.find s

let to_int m si =
  let s = S.map (fun e -> find_key_for e m) si in
  let r = CCList.find_idx (fun e -> S.equal e s) s_digits in
  match r with
  | None -> failwith "no results"
  | Some(i,_) -> i

let init_map = S.fold (fun e acc -> M.add e all_s acc) all_s M.empty

let two_chars l s2 =
  M.mapi (fun k s ->
    if k = 'c' || k = 'f'
    then S.inter s s2
    else S.diff s s2) l

let three_chars l s2 =
  M.mapi (fun k s ->
    if k = 'c' || k = 'f' || k = 'a'
    then S.inter s s2
    else S.diff s s2) l

let four_chars l s2 =
  M.mapi (fun k s ->
    if k = 'c' || k = 'f' || k = 'b' || k = 'd'
    then S.inter s s2
    else S.diff s s2) l

let five_chars l s2 =
  let e = S.diff all_s s2 in
  M.mapi (fun k s ->
    if k = 'a' || k = 'd' || k = 'g'
    then S.diff s e
    else s) l

let six_chars l s2 =
  let e = S.choose (S.diff all_s s2) in
  M.mapi (fun k s ->
    if k = 'c' || k = 'd' || k = 'e'
    then s
    else S.remove e s) l

let unique l =
  let aux l k s2 =
    M.mapi (fun k2 s ->
      if k2 = k
      then s
      else S.diff s s2) l in

  M.fold (fun k s acc ->
    if S.cardinal s = 0
    then failwith "No solution"
    else if S.cardinal s = 1
    then aux acc k s
    else acc) l l

let rec unique_rule l =
  let l2 = unique l in
  if l2 <> l
  then unique_rule l2
  else l2

let n_rules l s =
  match S.cardinal s with
  | 0 -> failwith "should not be empty"
  | 1 -> failwith "should not be one"
  | 2 -> two_chars l s
  | 3 -> three_chars l s
  | 4 -> four_chars l s
  | 5 -> five_chars l s
  | 6 -> six_chars l s
  | 7 -> l
  | _ -> failwith "should be > 1 & < 8"

let solve s =
  let m = init_map in
  let l =
    CCString.split ~by:" | " s
    |> CCList.map (CCString.split ~by:" ")
    |> CCList.map (CCList.map (fun s -> S.of_list (CCString.to_list s)))
  in match l with
     | [] | _ :: [] | _ :: _ :: _ :: _ -> failwith "error parsing input"
     | signals :: res :: [] ->
         m
         |> CCList.fold_right (fun s acc -> n_rules acc s) (signals @ res)
         |> unique_rule 
         |> (fun result -> CCList.fold_left (fun acc s -> (to_int result s) + 10*acc) 0 res)

let test_input =
  "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"

let run input =
  input
  |> CCString.trim
  |> CCString.split ~by:"\n"
  |> CCList.map (fun t -> solve t)
  |> CCFun.flip (CCList.fold_right (fun s acc -> s + acc)) 0

let () =
  assert ((run test_input) = 61229);
  let input = Helpers.input 8 in
  print_int (run (Lwt_main.run input))
