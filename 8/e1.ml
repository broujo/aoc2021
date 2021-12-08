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
  |> CCList.map (fun s ->
      s
      |> CCString.split ~by:" | "
      |> (function | [] | _ :: [] -> failwith "error in parsing input" | _ :: b :: _ -> b)
      |> CCString.split ~by:" "
      |> (fun l -> CCList.fold_right (fun e acc ->
          let len = CCString.length e in
          print_string e; print_int len; print_string "\n";
          if (len = 2 || len = 4 || len = 7 || len = 3)
          then acc + 1
          else acc) l 0))
  |> (fun l -> CCList.fold_right (fun e acc -> e + acc) l 0)
  |> (fun e -> print_int e; print_string "\n"; e)


let () =
  assert ((run test_input) = 26);
  let input = Helpers.input 8 in
  print_int (run (Lwt_main.run input))
