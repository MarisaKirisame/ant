open Value
open Word
open State
open Pattern

type pattern_subst_map = pattern Array.t
type pattern_subst_cek = pattern_subst_map cek
type value_subst_map = value Array.t
type value_subst_cek = value_subst_map cek

let rec unify (x : pattern) (y : pattern) : pattern =
  match (x, y) with
  | PVar _, p | p, PVar _ -> p
  | PNode (lx, cx), PNode (ly, cy) ->
      if not (Word.equal lx ly) then failwith "unify: label mismatch";
      if List.length cx <> List.length cy then failwith "unify: arity mismatch";
      PNode (lx, List.map2 unify cx cy)

let rec compose_pattern_aux (p : pattern) (s : pattern list) : pattern * pattern list =
  match p with
  | PVar _ -> ( match s with sh :: st -> (sh, st) | [] -> failwith "hole count mismatch")
  | PNode (label, children) ->
      let children', rest =
        List.fold_left
          (fun (acc, s) child ->
            let child', s = compose_pattern_aux child s in
            (acc @ [ child' ], s))
          ([], s) children
      in
      (PNode (label, children'), rest)

let compose_pattern (p : pattern) (s : pattern list) : pattern =
  let p', rest = compose_pattern_aux p s in
  if rest <> [] then failwith "hole count mismatch";
  p'

let rec subst_value (s : value_subst_cek) (v : value) : value =
  match v with
  | Reference r ->
      let sm = cek_get s r.src in
      Array.get sm r.hole_idx
  | Node (label, children) -> Node (label, List.map (subst_value s) children)

let rec value_match_pattern_aux (v : value) (p : pattern) : value list option =
  match p with
  | PVar _ -> Some [ v ]
  | PNode (label, children) -> (
      match v with
      | Reference _ -> None
      | Node (vlabel, vchildren) ->
          if not (Word.equal label vlabel) then None
          else if List.length children <> List.length vchildren then None
          else
            let rec loop acc ps vs =
              match (ps, vs) with
              | [], [] -> Some acc
              | p :: ps, v :: vs -> (
                  match value_match_pattern_aux v p with None -> None | Some bindings -> loop (acc @ bindings) ps vs)
              | _ -> None
            in
            loop [] children vchildren)

let value_match_pattern (v : value) (p : pattern) : value_subst_map option =
  Option.map Array.of_list (value_match_pattern_aux v p)

let rec pattern_to_value_aux (p : pattern) src (hole_idx : int ref) : value =
  match p with
  | PVar _ ->
      let r = Reference { src; hole_idx = !hole_idx } in
      hole_idx := !hole_idx + 1;
      r
  | PNode (label, children) -> Node (label, List.map (fun c -> pattern_to_value_aux c src hole_idx) children)

let pattern_to_value (p : pattern cek) : value cek = maps_ek (fun p s -> pattern_to_value_aux p s (ref 0)) p

(* unify pattern and value, building a substitution map for pattern *)
let rec unify_vp_aux (v : value) (p : pattern) (s : pattern_subst_cek) : unit =
  match p with
  | PVar _ -> ()
  | PNode (label, children) -> (
      match v with
      | Reference r ->
          let sm = cek_get s r.src in
          let hole_value = Array.get sm r.hole_idx in
          Array.set sm r.hole_idx (unify hole_value p)
      | Node (vlabel, vchildren) ->
          if not (Word.equal label vlabel) then failwith "unify_vp_aux: label mismatch";
          if List.length children <> List.length vchildren then failwith "unify_vp_aux: arity mismatch";
          List.iter2 (fun v p -> unify_vp_aux v p s) vchildren children)

let unify_vp (v : value cek) (p : pattern cek) (s : pattern_subst_cek) : pattern_subst_cek =
  let _ = zipwith_ek (fun v p -> unify_vp_aux v p s) v p in
  s

let value_match_pattern_ek (v : value cek) (p : pattern cek) : value_subst_cek option =
  Option.bind (zip_ek v p) (fun vp -> option_ek_to_ek_option (map_ek (fun (v, p) -> value_match_pattern v p) vp))

let can_step_through (step : step) (state : state) : bool =
  step.src.c.pc = state.c.pc && Option.is_some (value_match_pattern_ek state step.src)

let step_through (step : step) (state : state) : state =
  assert (step.src.c.pc = state.c.pc);
  let subst = Option.get (value_match_pattern_ek state step.src) in
  map_ek (subst_value subst) step.dst

let string_of_pat (p : pattern) : string = Pattern.string_of_pattern p
let string_of_pattern (p : pattern) : string = Pattern.string_of_pattern p
let bracket x = "(" ^ x ^ ")"

let string_of_step (step : step) : string =
  let src = pattern_to_value step.src in
  let dst = step.dst in
  bracket (string_of_cek src ^ " =>" ^ string_of_int step.sc ^ " " ^ string_of_cek dst)

let compose_step_step_through_slot = Profile.register_slot Profile.memo_profile "compose_step.step_through"
let unify_vp_slot = Profile.register_slot Profile.memo_profile "unify_vp"

let compose_step (x : step) (y : step) : step =
  if not (x.dst.c.pc = y.src.c.pc) then (
    print_endline "cannot compose steps:";
    print_endline ("x step: " ^ string_of_step x);
    print_endline ("y step: " ^ string_of_step y));
  assert (x.dst.c.pc = y.src.c.pc);
  let pattern_to_subst_map (p : pattern) : pattern_subst_map =
    let rec loop p acc =
      match p with
      | PVar _ -> PVar 1 :: acc
      | PNode (_, children) -> List.fold_left (fun acc c -> loop c acc) acc children
    in
    Array.of_list (List.rev (loop p []))
  in
  let s = Profile.with_slot unify_vp_slot (fun _ -> unify_vp x.dst y.src (map_ek pattern_to_subst_map x.src)) in
  let src = zipwith_ek (fun p s -> compose_pattern p (Array.to_list s)) x.src s in
  let subst : value_subst_cek =
    maps_ek
      (fun p s ->
        let hole_idx = ref 0 in
        Array.map (fun p -> pattern_to_value_aux p s hole_idx) p)
      s
  in
  let dst = map_ek (subst_value subst) x.dst in
  let dst = Profile.with_slot compose_step_step_through_slot (fun _ -> step_through y dst) in
  { src; dst; sc = x.sc + y.sc; hit = 0; insert_time = 0 }

let make_step (value : state) (resolved : bool cek) m : step =
  let pattern_of_value_root (v : value) : pattern =
    match v with
    | Reference _ -> make_pvar 1
    | Node (label, children) -> PNode (label, List.map (fun _ -> make_pvar 1) children)
  in
  let src = zipwith_ek (fun v resolved -> if resolved then pattern_of_value_root v else make_pvar 1) value resolved in
  let w = make_world (pattern_to_value src) m in
  w.state.c.step w;
  let dst = w.state in
  { src; dst; sc = 1; hit = 0; insert_time = 0 }

let value_equal (x : value) (y : value) : bool = Value.equal_value x y

let state_equal (x : state) (y : state) : bool =
  x.c.pc = y.c.pc && List.equal value_equal (Dynarray.to_list x.e) (Dynarray.to_list y.e) && value_equal x.k y.k
