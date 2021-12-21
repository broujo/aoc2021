(* https://stackoverflow.com/questions/3989776/transpose-of-a-list-of-lists *)
let rec transpose l =
  match l with
  | [] -> []
  | [] :: xss -> transpose xss
  | (x::xs) :: xss ->
      (x :: CCList.map CCList.hd xss) :: transpose (xs :: CCList.map CCList.tl xss)

let product row1 col1 =
  CCList.map2 ( * ) row1 col1
  |> CCList.fold_left (+) 0

let mult_mat mat1 mat2 =
  let columns = transpose mat2 in
  CCList.map (fun row -> CCList.map (product row) columns) mat1

let mult_vect mat (x,y,z) =
  mult_mat mat [[x];[y];[z]]
  |> CCList.flatten
  |> (function | x :: y :: z :: [] -> (x,y,z) | _ -> failwith "error in mult_vect")

type angle =
  | Pi0
  | Pi1
  | Pi2
  | Pi3

let all_angles = [Pi0; Pi1; Pi2; Pi3]

let cos a =
  match a with
  | Pi0 -> 1
  | Pi1
  | Pi3 -> 0
  | Pi2 -> -1

let sin a =
  match a with
  | Pi0
  | Pi2 -> 0 
  | Pi1 -> 1
  | Pi3 -> -1

let rotx a =
  [[1; 0    ; 0       ];
   [0; cos a; -(sin a)];
   [0; sin a;  cos a  ]]

let roty a =
  [[cos a   ; 0; sin a];
   [0       ; 1; 0    ];
   [-(sin a); 0; cos a]]

let rotz a =
  [[cos a; -(sin a); 0];
   [sin a; cos a   ; 0];
   [0    ; 0       ; 1]]

let all_rot_mat =
  let al = all_angles in
  let bl = all_angles in
  let cl = all_angles in
  CCList.cartesian_product [al; bl; cl]
  |> CCList.map (function
    | a :: b :: c :: [] ->
        mult_mat ( mult_mat (rotx a) (roty b)) (rotz c)
    | _ -> failwith "something went wrongg")
  |> CCList.sort_uniq ~cmp:compare

let make_vectors sensor =
  sensor
  |> CCList.diagonal
  |> CCList.flat_map (fun (a,b) -> [(a,b); (b,a)])
  |> CCList.map (fun (((x1,y1,z1) as a),((x2,y2,z2) as b)) -> (a,b,(x2-x1, y2-y1, z2-z1))) 

let inter_map ?(eq=(=)) f v1 v2 =
  CCList.filter_map (fun e2 -> 
    let e1 = CCList.find_opt (eq e2) v1 in
    CCOption.map (fun x -> f x e2) e1
  ) v2

let diff_vect (x1,y1,z1) (x2,y2,z2) =
  (x1 - x2, y1 - y2, z1 - z2)

let add_vect (x1,y1,z1) (x2,y2,z2) =
  (x1 + x2, y1 + y2, z1 + z2)

let find_inter_rotate sensor1 sensor2 =
  let pair1 = make_vectors sensor1 in
  let pair2 = make_vectors sensor2 in

  let ret = CCList.find_map (fun rot ->
    let pair2_rot = CCList.map (fun (a,b,v) -> (a,b,(mult_vect rot v))) pair2 in
    let commons = inter_map ~eq:(fun (_,_,v1) (_,_,v2) -> v1 = v2) (fun (a,_,_) (a1,_,_) -> a,(mult_vect rot a1)) pair1 pair2_rot in
    if CCList.length commons >= (66*2)
    then Some(rot,(CCList.sort_uniq ~cmp:compare commons))
    else None) all_rot_mat
  in CCOption.map (fun (rot,commons) ->
    match commons with
    | [] -> failwith "Impossible case"
    | (as0, as1) :: _ -> let s1_s0 = diff_vect as0 as1 in
                         rot,s1_s0) ret

let solve2 sensors =
  let rec aux solved found sensors =
    print_string "sensors length: "; print_int (CCList.length sensors); print_string "\n";
    flush stdout;
    match solved,sensors with
    | _,[] -> found
    | [],_ -> failwith "no solution found :'("
    | t :: q, _ -> begin
        let rrl = CCList.filter_map (fun si ->
          find_inter_rotate t si |>
          CCOption.map (fun (a,b) -> (si,a,b))) sensors in
        let sensors' = CCList.fold_right (fun (si, _, _) acc -> CCList.remove ~eq:(=) ~key:si acc) rrl sensors in
        let rrl_s0 =
          CCList.map (fun (si, rot, sensor) ->
              let si_s0 = CCList.map (fun v -> add_vect sensor (mult_vect rot v)) si in
              si_s0) rrl in
        let found' = (CCList.map (fun (_, _, sensor) -> sensor) rrl) @ found in
        let solved' = q @ rrl_s0 in
        aux solved' found' sensors'
    end in
  match sensors with
  | [] -> failwith "no solution"
  | _ :: [] -> [(0,0,0)]
  | t :: q -> aux [t] [(0,0,0)] q

let manhattan (s1, s2) =
  let dx, dy, dz = diff_vect s1 s2 in
  (CCInt.abs dx) + (CCInt.abs dy) + (CCInt.abs dz)
    
(* {{{ test_input *)
let test_input =
  "--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734
390,-675,-793
-537,-823,-458
-485,-357,347
-345,-311,381
-661,-816,-575
-876,649,763
-618,-824,-621
553,345,-567
474,580,667
-447,-329,318
-584,868,-557
544,-627,-890
564,392,-477
455,729,728
-892,524,684
-689,845,-530
423,-701,434
7,-33,-71
630,319,-379
443,580,662
-789,900,-551
459,-707,401

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
95,138,22
-476,619,847
-340,-569,-846
567,-361,727
-460,603,-452
669,-402,600
729,430,532
-500,-761,534
-322,571,750
-466,-666,-811
-429,-592,574
-355,545,-477
703,-491,-529
-328,-685,520
413,935,-424
-391,539,-444
586,-435,557
-364,-763,-893
807,-499,-711
755,-354,-619
553,889,-390

--- scanner 2 ---
649,640,665
682,-795,504
-784,533,-524
-644,584,-595
-588,-843,648
-30,6,44
-674,560,763
500,723,-460
609,671,-379
-555,-800,653
-675,-892,-343
697,-426,-610
578,704,681
493,664,-388
-671,-858,530
-667,343,800
571,-461,-707
-138,-166,112
-889,563,-600
646,-828,498
640,759,510
-630,509,768
-681,-892,-333
673,-379,-804
-742,-814,-386
577,-820,562

--- scanner 3 ---
-589,542,597
605,-692,669
-500,565,-823
-660,373,557
-458,-679,-417
-488,449,543
-626,468,-788
338,-750,-386
528,-832,-391
562,-778,733
-938,-730,414
543,643,-506
-524,371,-870
407,773,750
-104,29,83
378,-903,-323
-778,-728,485
426,699,580
-438,-605,-362
-469,-447,-387
509,732,623
647,635,-688
-868,-804,481
614,-800,639
595,780,-596

--- scanner 4 ---
727,592,562
-293,-554,779
441,611,-461
-714,465,-776
-743,427,-804
-660,-479,-426
832,-632,460
927,-485,-438
408,393,-506
466,436,-512
110,16,151
-258,-428,682
-393,719,612
-211,-452,876
808,-476,-593
-575,615,604
-485,667,467
-680,325,-822
-627,-443,-432
872,-547,-609
833,512,582
807,604,487
839,-516,451
891,-625,532
-652,-548,-490
30,-46,-14"

(* }}} *)

let run input =
  input
  |> CCString.split ~by:"\n\n"
  |> CCList.map CCString.lines
  |> CCList.map CCList.tl
  |> CCList.map (CCList.map (CCString.split ~by:","))
  |> CCList.map (CCList.map (CCList.map int_of_string))
  |> CCList.map (CCList.map (function | a :: b :: c :: [] -> (a,b,c) | _ -> failwith "fail parse"))
  |> solve2
  |> CCList.diagonal
  |> CCList.map manhattan
  |> CCList.fold_left max CCInt.min_int
 

let () =
  assert (run test_input = 3621);
  let input = Helpers.input 19 in
  print_int (run (Lwt_main.run input))
