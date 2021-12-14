module M = CCMap.Make(CCString)
module S = CCSet.Make(CCString)

let is_lower s =
  CCString.lowercase_ascii s = s

let make_path m e1 e2 =
  m
  |> M.update e2 (function
    | None -> Some(S.add e1 S.empty)
    | Some(s) -> Some(S.add e1 s))
  |> M.update e1 (function
    | None -> Some(S.add e2 S.empty)
    | Some(s) -> Some(S.add e2 s))

let add_line l m =
  match l with
  | e1 :: e2 :: [] -> make_path m e1 e2
  | _ -> failwith "list should have 2 elements"

let rec find_all_path m start endd acc =
  if start = endd then acc+1
  else 
    let s = M.get start m in
    match s with
    | None -> acc
    | Some(s) ->
        let m = if is_lower start then M.remove start m else m in
        S.fold (fun e acc -> find_all_path m e endd acc) s acc

let test_input_1 =
  "start-A
start-b
A-c
A-b
b-d
A-end
b-end"

let test_input_2 =
  "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"

let test_input_3 =
  "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW"

let run input =
  input
  |> CCString.lines
  |> CCList.map (CCString.split ~by:"-")
  |> CCFun.flip (CCList.fold_right add_line) M.empty
  |> (fun m -> find_all_path m "start" "end" 0)

let () =
  assert (run test_input_1 = 10);
  assert (run test_input_2 = 19);
  assert (run test_input_3 = 226);
  let input = Helpers.input 12 in
  print_int (run (Lwt_main.run input))
