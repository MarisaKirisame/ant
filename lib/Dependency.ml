open Value
open Word
open Words
open BatFingerTree
open State
open Pattern

type pattern_subst_map = pattern Array.t
type pattern_subst_cek = pattern_subst_map cek
type value_subst_map = value Array.t
type value_subst_cek = value_subst_map cek

let string_of_words (w : words) : string = Generic.to_list w |> List.map Word.to_string |> String.concat ""

let string_of_pattern (p : pattern) : string =
  Generic.to_list p
  |> List.map (fun pat -> match pat with PVar n -> "H(" ^ string_of_int n ^ ")" | PCon w -> string_of_words w)
  |> String.concat ""

let rec unify (x : pattern) (y : pattern) : pattern =
  let return z = z in
  if pattern_is_empty x then (
    assert (pattern_is_empty y);
    return Generic.empty)
  else (
    assert (not (pattern_is_empty y));
    let xh, xt = pattern_front_exn x in
    let yh, yt = pattern_front_exn y in
    match (xh, yh) with
    | PVar xh, _ ->
        let yl, yr = pattern_slice y xh in
        return (pattern_append yl (unify xt yr))
    | _, PVar yh ->
        let xl, xr = pattern_slice x yh in
        return (pattern_append xl (unify xr yt))
    | PCon xh, PCon yh ->
        let xl = Words.length xh in
        let yl = Words.length yh in
        if xl < yl then (
          let yhh, yht = Words.slice_length yh xl in
          assert (Words.equal xh yhh);
          return (pattern_cons_unsafe (PCon xh) (unify xt (pattern_cons_unsafe (PCon yht) yt))))
        else if xl > yl then (
          let xhh, xht = Words.slice_length xh yl in
          assert (Words.equal xhh yh);
          return (pattern_cons_unsafe (PCon xhh) (unify (pattern_cons_unsafe (PCon xht) xt) yt)))
        else (
          assert (xl = yl);
          assert (Words.equal xh yh);
          return (pattern_cons_unsafe (PCon xh) (unify xt yt))))

let rec compose_pattern p s =
  if pattern_is_empty p then match s with [] -> Generic.empty | _ -> failwith "hole count mismatch"
  else
    let ph, pt = pattern_front_exn p in
    match ph with
    | PVar _ -> (
        match s with sh :: st -> pattern_append sh (compose_pattern pt st) | [] -> failwith "hole count mismatch")
    | PCon ph -> pattern_cons (PCon ph) (compose_pattern pt s)

let rec subst_value (s : value_subst_cek) (v : value) : value =
  let lhs, rhs = Generic.split ~monoid:Value.monoid ~measure:Value.measure (fun m -> Option.is_none m.full) v in
  assert (Option.is_some (Generic.measure ~monoid:Value.monoid ~measure:Value.measure lhs).full);
  match Generic.front rhs ~monoid:Value.monoid ~measure:Value.measure with
  | None -> lhs
  | Some (rest, Reference r) ->
      let sm = cek_get s r.src in
      let sub_v = Array.get sm r.hole_idx in
      Value.append lhs (Value.append (Value.slice sub_v r.offset r.values_count) (subst_value s rest))
  | Some (_, Word _) -> failwith "impossible in subt_value"

let rec value_match_pattern_aux (v : value) (p : pattern) : value list option =
  let return x = x in
  assert ((Value.summary v).degree = (pattern_measure p).degree);
  assert ((Value.summary v).degree = (Value.summary v).max_degree);
  assert ((pattern_measure p).degree = (pattern_measure p).max_degree);
  if pattern_is_empty p then (
    assert (Generic.is_empty v);
    return (Some []))
  else
    let ph, pt = pattern_front_exn p in
    match ph with
    | PVar ph ->
        let vh, vt = Value.pop_n v ph in
        assert ((Value.summary vh).degree = (Value.summary vh).max_degree);
        assert ((Value.summary vh).degree = ph);
        return (Option.map (fun vs -> vh :: vs) (value_match_pattern_aux vt pt))
    | PCon ph ->
        let pl = Words.length ph in
        let vh, vt =
          Generic.split ~monoid:Value.monoid ~measure:Value.measure
            (fun m -> not (match m.full with Some f -> f.length <= pl | None -> false))
            v
        in
        let m = Value.summary vh in
        let f = Option.get m.full in
        if f.length < pl then return None
        else (
          assert (f.length = pl);
          if f.hash = (Words.summary ph).hash then return (value_match_pattern_aux vt pt) else return None)

let value_match_pattern (v : value) (p : pattern) : value_subst_map option =
  let return x = x in
  return (Option.map (fun lst -> Array.of_list lst) (value_match_pattern_aux v p))

let words_to_value (w : words) : value = Generic.map ~monoid:Value.monoid ~measure:Value.measure (fun wt -> Word wt) w

let rec pattern_to_value_aux (p : pattern) src hole_idx : value =
  if Generic.is_empty p then Generic.empty
  else
    let ph, pt = pattern_front_exn p in
    match ph with
    | PVar n ->
        Value.append
          (Generic.singleton (Reference { src; hole_idx; offset = 0; values_count = n }))
          (pattern_to_value_aux pt src (hole_idx + 1))
    | PCon c -> Value.append (words_to_value c) (pattern_to_value_aux pt src hole_idx)

let pattern_to_value (p : pattern cek) : value cek = maps_ek (fun p s -> pattern_to_value_aux p s 0) p

(*todo: this code look a lot like value_match_pattern, is there ways to unify them?*)
(*unify pattern and value, building a substituion map for pattern*)
let rec unify_vp_aux (v : value) (p : pattern) (s : pattern_subst_cek) : pattern_subst_cek =
  assert ((Value.summary v).degree = (pattern_measure p).degree);
  assert ((Value.summary v).degree = (Value.summary v).max_degree);
  assert ((pattern_measure p).degree = (pattern_measure p).max_degree);
  let return x = x in
  if pattern_is_empty p then (
    assert (Generic.is_empty v);
    return s)
  else
    let ph, pt = pattern_front_exn p in
    match ph with
    | PVar ph ->
        let vh, vt = Value.pop_n v ph in
        assert ((Value.summary vh).degree = (Value.summary vh).max_degree);
        assert ((Value.summary vh).degree = ph);
        return (unify_vp_aux vt pt s)
    | PCon ph ->
        let pl = Words.length ph in
        let vh, vt =
          Generic.split ~monoid:Value.monoid ~measure:Value.measure
            (fun m -> not (match m.full with Some f -> f.length <= pl | None -> false))
            v
        in
        let m = Value.summary vh in
        let f = Option.get m.full in
        if f.length = 0 then
          let rest, r = Generic.front_exn ~monoid:Value.monoid ~measure:Value.measure v in
          match r with
          | Reference r ->
              let ph, pt = pattern_slice p r.values_count in
              let sm = cek_get s r.src in
              let unify_with = Array.get sm r.hole_idx in
              assert ((pattern_measure ph).degree = (pattern_measure ph).max_degree);
              let ph = if r.offset > 0 then pattern_cons (make_pvar r.offset) ph else ph in
              let needed = (pattern_measure unify_with).max_degree - (r.offset + r.values_count) in
              assert (needed >= 0);
              let ph = if needed > 0 then pattern_snoc ph (make_pvar needed) else ph in
              Array.set sm r.hole_idx (unify unify_with ph);
              return (unify_vp_aux rest pt s)
          | Word _ -> failwith "impossible in unify_vp_aux: get word"
        else if f.length < pl then (
          let phh, pht = Words.slice_length ph f.length in
          if not (f.hash = (Words.summary phh).hash) then (
            print_endline "should not happens:";
            print_endline ("vh: " ^ string_of_value vh);
            print_endline ("phh: " ^ string_of_words phh));
          assert (f.hash = (Words.summary phh).hash);
          return (unify_vp_aux vt (pattern_cons_unsafe (PCon pht) pt) s))
        else (
          assert (f.length = pl);
          assert (f.hash = (Words.summary ph).hash);
          return (unify_vp_aux vt pt s))

let unify_vp (v : value cek) (p : pattern cek) (s : pattern_subst_cek) : pattern_subst_cek =
  fold_ek (Option.get (zip_ek v p)) s (fun s (v, p) -> unify_vp_aux v p s)

let value_match_pattern_ek (v : value cek) (p : pattern cek) : value_subst_cek option =
  Option.bind (zip_ek v p) (fun vp -> option_ek_to_ek_option (map_ek (fun (v, p) -> value_match_pattern v p) vp))

let can_step_through (step : step) (state : state) : bool =
  step.src.c.pc = state.c.pc && Option.is_some (value_match_pattern_ek state step.src)

let step_through (step : step) (state : state) : state =
  assert (step.src.c.pc = state.c.pc);
  let subst = Option.get (value_match_pattern_ek state step.src) in
  let s = map_ek (subst_value subst) step.dst in
  { s with sc = state.sc + step.sc }

let string_of_pat (p : pat) : string =
  match p with PVar n -> "PVar(" ^ string_of_int n ^ ")" | PCon w -> "PCon(" ^ string_of_words w ^ ")"

let string_of_pattern (p : pattern) : string =
  "[" ^ String.concat ";" (List.map string_of_pat (Generic.to_list p)) ^ "]"

let string_of_step (step : step) : string =
  let src = pattern_to_value step.src in
  let dst = step.dst in
  "(" ^ string_of_cek src ^ " -> " ^ string_of_cek dst ^ ")"

let compose_step (x : step) (y : step) : step =
  (* Compose two recorded steps that share a program counter boundary by
     unifying x.dst with y.src, then replaying both with the resolved holes to
     produce a single wider fragment. *)
  if not (x.dst.c.pc = y.src.c.pc) then (
    print_endline "cannot compose steps:";
    print_endline ("x step: " ^ string_of_step x);
    print_endline ("y step: " ^ string_of_step y));
  assert (x.dst.c.pc = y.src.c.pc);
  let pattern_to_subst_map (p : pattern) : pattern_subst_map =
    let rec loop p =
      if Generic.is_empty p then []
      else
        let ph, pt = pattern_front_exn p in
        match ph with PVar n -> Generic.singleton (make_pvar n) :: loop pt | PCon _ -> loop pt
    in
    Array.of_list (loop p)
  in
  let s = unify_vp x.dst y.src (map_ek pattern_to_subst_map x.src) in
  let src =
    map_ek
      (fun (p, s) ->
        let s = Array.to_list s in
        compose_pattern p s)
      (Option.get (zip_ek x.src s))
  in
  let dst = { (pattern_to_value src) with sc = 0 } in
  let dst = step_through x dst in
  if not (can_step_through y dst) then (
    print_endline "cannot compose steps:";
    print_endline ("generalized pattern: " ^ string_of_cek (pattern_to_value src));
    print_endline ("x step: " ^ string_of_step x);
    print_endline ("intermediate: " ^ string_of_cek dst);
    print_endline ("y step: " ^ string_of_step y));
  assert (can_step_through y dst);
  let dst = step_through y dst in
  assert (dst.sc = x.sc + y.sc);
  { src; dst; sc = dst.sc }

let make_step (value : state) (resolved : bool cek) m : step =
  let src =
    map_ek
      (fun (v, resolved) ->
        assert ((Value.summary v).degree > 0);
        if resolved then
          let vt, vh = Generic.front_exn ~monoid:Value.monoid ~measure:Value.measure v in
          match vh with
          | Word vh ->
              Generic.of_list ~monoid:Pattern.monoid ~measure:Pattern.pat_measure
                (if (Value.summary vt).degree = 0 then [ PCon (Generic.singleton vh) ]
                 else [ PCon (Generic.singleton vh); make_pvar (Value.summary vt).degree ])
          | _ -> failwith "cannot make step"
        else Generic.singleton (make_pvar (Value.summary v).degree))
      (Option.get (zip_ek value resolved))
  in
  let w = make_world (pattern_to_value src) m in
  w.state.c.step w;
  let dst = w.state in
  { src; dst; sc = 1 }

let bracket x = "(" ^ x ^ ")"

let string_of_step (step : step) : string =
  let src = pattern_to_value step.src in
  let dst = step.dst in
  bracket (string_of_cek src ^ " => " ^ string_of_cek dst)

let value_equal (x : value) (y : value) : bool = Generic.equal equal_fg_et x y

let state_equal (x : state) (y : state) : bool =
  x.c.pc = y.c.pc && List.equal value_equal (Dynarray.to_list x.e) (Dynarray.to_list y.e) && value_equal x.k y.k
