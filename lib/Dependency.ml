open Value
open Word
open Words
open PlainTree
open State
open Pattern

type pattern_subst_map = pattern Array.t
type pattern_subst_cek = pattern_subst_map cek
type value_subst_map = value Array.t
type value_subst_cek = value_subst_map cek

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
    | PVar xh, PVar yh ->
        let m = min xh yh in
        let xl = xh - m in
        let yl = yh - m in
        let xt = if xl > 0 then pattern_cons (PVar xl) xt else xt in
        let yt = if yl > 0 then pattern_cons (PVar yl) yt else yt in
        return (pattern_cons (PVar m) (unify xt yt))
    | PVar xh, _ ->
        let yl, yr = pattern_slice y xh in
        return (pattern_append yl (unify xt yr))
    | _, PVar yh ->
        let xl, xr = pattern_slice x yh in
        return (pattern_append xl (unify xr yt))
    | PCon xh, PCon yh ->
        let xl = Words.length xh in
        let yl = Words.length yh in
        if xl < yl then
          let yhh, yht = Words.slice_length yh xl in
          (*assert (Words.equal_words xh yhh);*)
          return (pattern_cons (PCon xh) (unify xt (pattern_cons_unsafe (PCon yht) yt)))
        else if xl > yl then
          let xhh, xht = Words.slice_length xh yl in
          (*assert (Words.equal_words xhh yh);*)
          return (pattern_cons (PCon xhh) (unify (pattern_cons_unsafe (PCon xht) xt) yt))
        else (
          assert (xl = yl);
          (*assert (Words.equal_words xh yh);*)
          return (pattern_cons (PCon xh) (unify xt yt))))

let rec compose_pattern p s =
  if pattern_is_empty p then match s with [] -> Generic.empty | _ -> failwith "hole count mismatch"
  else
    let ph, pt = pattern_front_exn p in
    match ph with
    | PVar _ -> (
        match s with sh :: st -> pattern_append sh (compose_pattern pt st) | [] -> failwith "hole count mismatch")
    | PCon ph -> pattern_cons (PCon ph) (compose_pattern pt s)

let rec subst_value (s : value_subst_cek) (v : value) : value =
  if Generic.is_empty v then Generic.empty
  else
    match Generic.front_exn ~monoid:Value.monoid ~measure:Value.measure v with
    | rest, Words w -> Value.value_cons (Words w) (subst_value s rest)
    | rest, Reference r ->
        let sm = cek_get s r.src in
        let sub_v = Array.get sm r.hole_idx in
        Value.append (Value.slice sub_v r.offset r.values_count) (subst_value s rest)

let rec value_match_pattern_aux (v : value) (p : pattern) : value list option =
  (*assert (value_valid v);*)
  let return x = x in
  (*assert ((Value.summary v).degree = (pattern_measure p).degree);
  assert ((Value.summary v).degree = (Value.summary v).max_degree);
  assert ((pattern_measure p).degree = (pattern_measure p).max_degree);*)
  if pattern_is_empty p then (
    assert (Generic.is_empty v);
    return (Some []))
  else
    let ph, pt = pattern_front_exn p in
    match ph with
    | PVar ph ->
        let vh, vt = Value.pop_n v ph in
        (*assert ((Value.summary vh).degree = (Value.summary vh).max_degree);
        assert ((Value.summary vh).degree = ph);*)
        return (Option.map (fun vs -> vh :: vs) (value_match_pattern_aux vt pt))
    | PCon ph -> (
        assert (not (Generic.is_empty ph));
        match Value.unwords v ph with None -> return None | Some v -> return (value_match_pattern_aux v pt))

let value_match_pattern (v : value) (p : pattern) : value_subst_map option =
  (*assert (value_valid v);*)
  let return x = x in
  return (Option.map (fun lst -> Array.of_list lst) (value_match_pattern_aux v p))

let rec pattern_to_value_aux (p : pattern) src (hole_idx : int ref) : value =
  if Generic.is_empty p then Generic.empty
  else
    let ph, pt = pattern_front_exn p in
    match ph with
    | PVar n ->
        let r = Reference { src; hole_idx = !hole_idx; offset = 0; values_count = n } in
        hole_idx := !hole_idx + 1;
        Value.value_cons r (pattern_to_value_aux pt src hole_idx)
    | PCon c -> Value.value_cons (Words c) (pattern_to_value_aux pt src hole_idx)

let pattern_to_value (p : pattern cek) : value cek = maps_ek (fun p s -> pattern_to_value_aux p s (ref 0)) p

(*todo: this code look a lot like value_match_pattern, is there ways to unify them?*)
(*unify pattern and value, building a substituion map for pattern*)
let rec unify_vp_aux (v : value) (p : pattern) (s : pattern_subst_cek) : unit =
  (*assert ((Value.summary v).degree = (pattern_measure p).degree);
  assert ((Value.summary v).degree = (Value.summary v).max_degree);
  assert ((pattern_measure p).degree = (pattern_measure p).max_degree);*)
  (*assert (Pattern.pattern_valid p);*)
  let return x = x in
  if pattern_is_empty p then (
    assert (Generic.is_empty v);
    return ())
  else
    let ph, pt = pattern_front_exn p in
    match ph with
    | PVar ph ->
        let vh, vt = Value.pop_n v ph in
        (*assert ((Value.summary vh).degree = (Value.summary vh).max_degree);
        assert ((Value.summary vh).degree = ph);*)
        return (unify_vp_aux vt pt s)
    | PCon ph -> (
        match Generic.front_exn ~monoid:Value.monoid ~measure:Value.measure v with
        | rest, Words w ->
            let pl = Words.length ph in
            let m = Words.summary w in
            if m.length < pl then (
              let phh, pht = Words.slice_length ph m.length in
              if not (Lazy.force m.hash = Words.hash phh) then (
                print_endline "should not happens:";
                print_endline ("phh: " ^ string_of_words phh));
              assert (Lazy.force m.hash = Words.hash phh);
              return (unify_vp_aux rest (pattern_cons_unsafe (PCon pht) pt) s))
            else (
              assert (m.length >= pl);
              match Words.unwords w ph with
              | None -> failwith "unify_vp_aux: cannot unify"
              | Some wt ->
                  let rest = if Words.is_empty wt then rest else Value.value_cons (Words wt) rest in
                  return (unify_vp_aux rest pt s))
        | rest, Reference r ->
            let ph, pt = pattern_slice p r.values_count in
            let sm = cek_get s r.src in
            let unify_with = Array.get sm r.hole_idx in
            (*assert ((pattern_measure ph).degree = (pattern_measure ph).max_degree);*)
            let ph = if r.offset > 0 then pattern_cons (make_pvar r.offset) ph else ph in
            let needed = (pattern_measure unify_with).max_degree - (r.offset + r.values_count) in
            assert (needed >= 0);
            let ph = if needed > 0 then pattern_snoc ph (make_pvar needed) else ph in
            let hole_value = unify unify_with ph in
            (*assert (Pattern.pattern_valid unify_with);
              assert (Pattern.pattern_valid ph);
              assert (Pattern.pattern_valid hole_value);*)
            Array.set sm r.hole_idx hole_value;
            return (unify_vp_aux rest pt s))

let unify_vp (v : value cek) (p : pattern cek) (s : pattern_subst_cek) : pattern_subst_cek =
  let _ = zipwith_ek (fun v p -> unify_vp_aux v p s) v p in
  s

let value_match_pattern_ek (v : value cek) (p : pattern cek) : value_subst_cek option =
  Option.bind (zip_ek v p) (fun vp ->
      option_ek_to_ek_option
        (map_ek
           (fun (v, p) ->
             (*assert (Value.value_valid v);*)
             value_match_pattern v p)
           vp))

let can_step_through (step : step) (state : state) : bool =
  step.src.c.pc = state.c.pc && Option.is_some (value_match_pattern_ek state step.src)

let step_through (step : step) (state : state) : state =
  (*let _ = map_ek (fun v -> assert (Value.value_valid v)) step.dst in*)
  (*let _ = map_ek (fun v -> assert (Value.value_valid v)) state in*)
  let return x =
    (*let _ = map_ek (fun v -> assert (Value.value_valid v)) x in*)
    x
  in
  assert (step.src.c.pc = state.c.pc);
  let subst = Option.get (value_match_pattern_ek state step.src) in
  (*let _ = map_ek (fun v -> assert (Value.value_valid v)) step.dst in*)
  return (map_ek (subst_value subst) step.dst)

let string_of_pat (p : pat) : string =
  match p with PVar n -> "PVar(" ^ string_of_int n ^ ")" | PCon w -> "PCon(" ^ string_of_words w ^ ")"

let string_of_pattern (p : pattern) : string =
  "[" ^ String.concat ";" (List.map string_of_pat (Generic.to_list p)) ^ "]"

let string_of_step (step : step) : string =
  let src = pattern_to_value step.src in
  let dst = step.dst in
  "(" ^ string_of_cek src ^ " -> " ^ string_of_cek dst ^ ")"

let compose_step_step_through_slot = Profile.register_slot Profile.memo_profile "compose_step.step_through"
let unify_vp_slot = Profile.register_slot Profile.memo_profile "unify_vp"

let compose_step (x : step) (y : step) : step =
  (*let _ = map_ek (fun v -> assert (Value.value_valid v)) x.dst in*)
  (*let _ = map_ek (fun v -> assert (Value.value_valid v)) y.dst in*)
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
  let s = Profile.with_slot unify_vp_slot (fun _ -> unify_vp x.dst y.src (map_ek pattern_to_subst_map x.src)) in
  let src =
    zipwith_ek
      (fun p s ->
        (*assert (Pattern.pattern_valid p);
        Array.iter (fun sp -> assert (Pattern.pattern_valid sp)) s;*)
        let ret = compose_pattern p (Array.to_list s) in
        (*assert (Pattern.pattern_valid ret);*)
        ret)
      x.src s
  in
  let subst : value_subst_cek =
    maps_ek
      (fun p s ->
        let hole_idx = ref 0 in
        Array.map (fun p -> pattern_to_value_aux p s hole_idx) p)
      s
  in
  let dst = map_ek (subst_value subst) x.dst in
  (*if not (can_step_through y dst) then (
    print_endline "cannot compose steps:";
    print_endline ("generalized pattern: " ^ string_of_cek (pattern_to_value src));
    print_endline ("x step: " ^ string_of_step x);
    print_endline ("intermediate: " ^ string_of_cek dst);
    print_endline ("y step: " ^ string_of_step y));
  assert (can_step_through y dst);*)
  let dst = Profile.with_slot compose_step_step_through_slot (fun _ -> step_through y dst) in
  (*let _ = map_ek (fun v -> assert (Value.value_valid v)) dst in*)
  { src; dst; sc = x.sc + y.sc; hit = 0; insert_time = 0 }

let make_step (value : state) (resolved : bool cek) m : step =
  (*let _ = map_ek (fun v -> assert (Value.value_valid v)) value in*)
  let src =
    zipwith_ek
      (fun v resolved ->
        (*assert ((Value.summary v).degree > 0);*)
        if resolved then
          let vt, vh = Generic.front_exn ~monoid:Value.monoid ~measure:Value.measure v in
          match vh with
          | Words vh ->
              let vht, vhh = Generic.front_exn ~monoid:Words.monoid ~measure:Words.measure vh in
              let vt = if Generic.is_empty vht then vt else Value.value_cons (Words vht) vt in
              Generic.of_list ~monoid:Pattern.monoid ~measure:Pattern.pat_measure
                (if (Value.summary vt).degree = 0 then [ PCon (Generic.singleton vhh) ]
                 else [ PCon (Generic.singleton vhh); make_pvar (Value.summary vt).degree ])
          | _ -> failwith "cannot make step"
        else Generic.singleton (make_pvar (Value.summary v).degree))
      value resolved
  in
  let w = make_world (pattern_to_value src) m in
  (*let _ = map_ek (fun v -> assert (Value.value_valid v)) w.state in*)
  w.state.c.step w;
  let dst = w.state in
  (*let _ = map_ek (fun v -> assert (Value.value_valid v)) dst in*)
  { src; dst; sc = 1; hit = 0; insert_time = 0 }

let bracket x = "(" ^ x ^ ")"

let string_of_step (step : step) : string =
  let src = pattern_to_value step.src in
  let dst = step.dst in
  bracket (string_of_cek src ^ " =>" ^ string_of_int step.sc ^ " " ^ string_of_cek dst)

let value_equal (x : value) (y : value) : bool = Generic.equal equal_fg_et x y

let state_equal (x : state) (y : state) : bool =
  x.c.pc = y.c.pc && List.equal value_equal (Dynarray.to_list x.e) (Dynarray.to_list y.e) && value_equal x.k y.k
