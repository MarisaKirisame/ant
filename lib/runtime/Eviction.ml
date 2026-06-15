open State

type eviction_policy = { retain_ratio : float; kll_k : int }
type eviction_state = { mutable max_tree_size_seen : int }

let make_eviction_policy ~retain_ratio ~kll_k =
  if retain_ratio < 0.0 || retain_ratio > 1.0 then
    invalid_arg "Eviction.make_eviction_policy: retain_ratio must be in [0.0, 1.0]";
  if kll_k <= 0 then invalid_arg "Eviction.make_eviction_policy: kll_k must be positive";
  { retain_ratio; kll_k }

let init_eviction_state () = { max_tree_size_seen = 0 }

let update_max_tree_size_seen state ~current_size =
  if current_size > state.max_tree_size_seen then state.max_tree_size_seen <- current_size

let target_size_from_state ~policy ~state =
  let raw = ceil (policy.retain_ratio *. float_of_int state.max_tree_size_seen) in
  assert (raw >= 0.0);
  int_of_float raw

let score_step (step : State.step) : int =
  assert (step.sc >= 0);
  assert (step.hit >= 0);
  step.sc * step.hit

let rec iter_trie_steps (f : State.step -> unit) (trie : trie) : unit =
  match trie with
  | Leaf { step; _ } -> f step
  | Branch br ->
      (match br.var with None -> () | Some var -> iter_trie_steps f var);
      Children.iter br.const ~f:(iter_trie_steps f)

let iter_memo_steps (f : State.step -> unit) (memo : State.memo) : unit =
  Array.iter (function None -> () | Some trie -> iter_trie_steps f trie) memo

let rec trie_size (trie : trie) : int =
  match trie with
  | Leaf _ -> 1
  | Branch br ->
      let var_count = match br.var with None -> 0 | Some var -> trie_size var in
      let const_count =
        let acc = ref 0 in
        Children.iter br.const ~f:(fun child -> acc := !acc + trie_size child);
        !acc
      in
      var_count + const_count

let memo_size (memo : State.memo) : int =
  Array.fold_left (fun acc opt -> match opt with None -> acc | Some trie -> acc + trie_size trie) 0 memo

let stream_scores_from_trie ~kll trie = iter_trie_steps (fun step -> Kll.add kll (score_step step)) trie

let stream_scores_from_memo ~kll memo =
  Array.iter (function None -> () | Some trie -> stream_scores_from_trie ~kll trie) memo

let eviction_fraction_for_target ~target_size memo =
  let total = memo_size memo in
  assert (total > 0);
  assert (target_size < total);
  let to_evict = total - target_size in
  assert (to_evict > 0);
  float_of_int to_evict /. float_of_int total

let threshold_for_fraction_from_memo ~kll_k ~evict_fraction memo =
  assert (evict_fraction >= 0.0);
  assert (evict_fraction <= 1.0);
  let kll = Kll.create ~k:kll_k () in
  stream_scores_from_memo ~kll memo;
  match Kll.quantile kll ~q:evict_fraction with Some threshold -> threshold | None -> assert false

let max_sc_of_trie (trie : trie) : int = match trie with Leaf { max_sc; _ } -> max_sc | Branch br -> br.max_sc

let prune_memo_to_target ~kll_k ~evict_fraction memo =
  assert (evict_fraction >= 0.0);
  assert (evict_fraction <= 1.0);
  let threshold = threshold_for_fraction_from_memo ~kll_k ~evict_fraction memo in

  (* TODO: compress trie paths after pruning to reduce depth and memory overhead. *)
  let rec prune_trie (trie : trie) : trie option =
    match trie with
    | Leaf { prefix; step; _ } ->
        let s = score_step step in
        if s >= threshold then Some (Leaf { prefix; step; max_sc = step.sc }) else None
    | Branch br ->
        let var' = match br.var with None -> None | Some var -> prune_trie var in
        let const' = Children.create () in
        Children.iteri br.const ~f:(fun ~key ~data:child ->
            match prune_trie child with None -> () | Some kept -> Children.set const' key kept);
        let has_var = Option.is_some var' in
        let has_const = Children.length const' > 0 in
        if (not has_var) && not has_const then None
        else
          let max_sc =
            let var_sc = match var' with None -> 0 | Some var -> max_sc_of_trie var in
            let const_sc =
              let acc = ref 0 in
              Children.iter const' ~f:(fun child -> acc := max !acc (max_sc_of_trie child));
              !acc
            in
            max var_sc const_sc
          in
          Some
            (Branch { creator = br.creator; degree = br.degree; prefix = br.prefix; var = var'; const = const'; max_sc })
  in

  Array.map (function None -> None | Some trie -> prune_trie trie) memo

let batch_evict_memo ~policy ~state memo =
  let current_size = memo_size memo in
  update_max_tree_size_seen state ~current_size;
  let target_size = target_size_from_state ~policy ~state in
  if current_size <= target_size then memo
  else
    let evict_fraction = eviction_fraction_for_target ~target_size memo in
    prune_memo_to_target ~kll_k:policy.kll_k ~evict_fraction memo
