open Parser

module CRes = struct
  type t = C.t * CCInt.t
  let compare (p0,n0) (p1,n1) =
    match C.compare p0 p1 with
    | 0 -> CCInt.compare n0 n1
    | c -> c
end

module MRes = CCMap.Make(CRes)
module MC = CCMap.Make(CCChar)

(* Data tansform_map is a graph Pair -> Pair * Pair
 *                                  M
 * map_res is a map (Pair,Int) -> (map CCChar -> Int))
 *                MRes                    MC
 *
 * Exemple:
 * transform_map: NN -> C
 * map_res: NN -> ( 0 -> (N -> 2))
 *                ( 1 -> (N -> 2) (C -> 1))
 *                ( 2 -> (N -> 2) (C -> 2) (B -> 1))
 *
 *          NC -> ( 0 -> (N -> 1) (C -> 1))
 *                ( 1 -> (N -> 1) (B -> 1) (C -> 1))
 *
 *          CN -> ( 0 -> (C -> 1) (N -> 1))
 *                ( 1 -> (C -> 2) (N -> 1))
 *
 * => NN 2 = add (N,transform_map NN)1 (transform_map NN),N1
 *         = add_pair NC1 CN1
 *         = (add_mc NC1 CN1) - C
 *         = (N -> 1) (B -> 1) (C -> 1) (C -> 2) (N -> 1) - (C)
 *          = 2N + B + 3C - C
 *          = 2N + B + 2C
 *
 * => NN n = add NC(n-1) CN(n-1)
 *)

let add_mc ab bc =
  let f _ e =
    match e with
    | `Left n -> Some(n)
    | `Right n -> Some(n)
    | `Both(n1, n2) -> Some(n1 + n2) in
  MC.merge_safe ~f ab bc

let add_pair mc_ac mc_cb c =
  let mc_ab = add_mc mc_ac mc_cb in
  MC.update c (function | None -> failwith "cannot be none" | Some v -> Some (v-1)) mc_ab

let mres_final mres ((a,b) as ab) n =
  let mc = add_mc (MC.add a 1 MC.empty) (MC.add b 1 MC.empty) in
  MRes.update (ab,n) (function (* I could update all n until 0 here.. might do if the code is too slow *)
    | Some _ -> failwith "Should not be aleady present - already checked"
    | None -> Some mc) mres

let rec solve_pair tmap rmap ((a,b) as ab) n =
  match MRes.get (ab,n) rmap with
  | Some _ -> rmap
  | None ->
      if n = 0
      then mres_final rmap ab n
      else begin match M.get ab tmap with
      | None -> mres_final rmap ab n
      | Some c ->
          let rmap = solve_pair tmap rmap (a,c) (n-1) in
          let rmap = solve_pair tmap rmap (c,b) (n-1) in
          let mc_ac = MRes.find ((a,c),(n-1)) rmap in
          let mc_cb = MRes.find ((c,b),(n-1)) rmap in
          let mc_ab = add_pair mc_ac mc_cb c in
          MRes.add (ab,n) mc_ab rmap
      end

let rec make_mres n l tmap mres =
  match l with
  | [] | _ :: [] -> mres
  | a :: b :: q -> make_mres n (b :: q) tmap (solve_pair tmap mres (a,b) n)

let rec make_mc n l tmap mres accmc =
  match l with
  | [] | _ :: [] -> accmc
  | a :: b :: [] ->
      let ab = MRes.find ((a,b),n) mres in
      add_mc accmc ab
  | a :: b :: c :: q ->
      let ab = MRes.find ((a,b),n) mres in
      let accmc = add_pair accmc ab b in
      make_mc n (b :: c :: q) tmap mres accmc

let solve n l tmap =
  let mres = make_mres n l tmap MRes.empty in
  let mcc = make_mc n l tmap mres MC.empty in
  MC.fold (fun _ n (mi, ma) ->
    ((if n < mi then n else mi), (if n > ma then n else ma))) mcc (CCInt.max_int,CCInt.min_int)
  |> (fun (a,b) -> b - a)
  

let test_input = "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"

let run input =
  let input_l,transform_map = parse input in
  solve 40 input_l transform_map

let () =
  assert ((run test_input) = 2188189693529);
  let input = Helpers.input 14 in
  print_int (run (Lwt_main.run input))
