module M = CCMap.Make(Int)

let update_fish n v m =
  M.update
    v
    (fun e ->
      match e with
      | None -> Some(n)
      | Some(i) -> Some(i + n))
    m

let rec solve n m =
  match n with
  | 0 -> M.fold (fun _ n res -> n + res) m 0
  | _ ->
      let newm = M.fold (fun k n nm ->
        if k = 0
        then update_fish n 8 (update_fish n 6 nm)
        else update_fish n (k-1) nm) m M.empty in
      solve (n-1) newm
 
let test_input = "3,4,3,1,2"

let run input =
  let m = M.empty in
  input
  |> CCString.trim
  |> CCString.split ~by:","
  |> CCList.map (CCInt.of_string_exn)
  |> (fun l -> CCList.fold_right (update_fish 1) l m)
  |> solve 80

let () =
  assert ((run test_input) == 5934);
  let input = Helpers.input 6 in
  print_int (run (Lwt_main.run input))

