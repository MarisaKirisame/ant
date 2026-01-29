open BatFingerTree
open Word
module Hasher = Hash.MCRC32C
open Base
module Dynarray = Stdlib.Dynarray

(*module Hasher = Hash.MCRC32*)
(*module Hasher = Hash.SL2*)
(*module Hasher = Hash.DebugHash*)
module Hashtbl = Core.Hashtbl
open Value
open State
open Common
open Core

let log x = print_endline x
let log x = ignore x

(* Just have Word.t. We could make Word a finger tree of Word.t but that would cost lots of conversion between two representation. *)
type words = seq
type exec_result = { words : words; step : int; without_memo_step : int }

let source_to_string (src : source) = match src with E i -> "E" ^ string_of_int i | K -> "K"

let get_value (state : 'a cek) (src : source) : 'a =
  match src with
  | E i ->
      assert (i < Dynarray.length state.e);
      Dynarray.get state.e i
  | K -> state.k

let set_value (state : 'a cek) (src : source) (v : 'a) : unit =
  match src with
  | E i ->
      assert (i < Dynarray.length state.e);
      Dynarray.set state.e i v
  | K -> state.k <- v

let rec subst (resolve : source -> seq option) (x : seq) : seq =
  if Generic.is_empty x then x
  else
    let xt, xh = Generic.front_exn ~monoid ~measure x in
    match xh with
    | Words xh -> Value.value_cons (Words xh) (subst resolve xt)
    | Reference r -> Value.append (subst_reference resolve r) (subst resolve xt)

and subst_reference (resolve : source -> seq option) (r : reference) : seq =
  match resolve r.src with Some seq -> slice seq r.offset r.values_count | None -> Generic.singleton (Reference r)

let subst_state (x : state) (resolve : source -> seq option) : state =
  let c = x.c in
  let e = Dynarray.map (fun v -> subst resolve v) x.e in
  let k = subst resolve x.k in
  { c; e; k }

let dyn_array_update (f : 'a -> 'a) (arr : 'a Dynarray.t) : unit =
  let len = Dynarray.length arr in
  for i = 0 to len - 1 do
    Dynarray.set arr i (f (Dynarray.get arr i))
  done

let dyn_array_rev_update (f : 'a -> 'a) (arr : 'a Dynarray.t) : unit =
  let len = Dynarray.length arr in
  for i = len - 1 downto 0 do
    Dynarray.set arr i (f (Dynarray.get arr i))
  done

let rec val_refs_aux (x : value) (rs : reference list) : reference list =
  match Generic.front ~monoid ~measure x with
  | None -> rs
  | Some (rest, Words w) -> val_refs_aux rest rs
  | Some (rest, Reference r) -> val_refs_aux rest (r :: rs)

let state_refs (state : state) : reference list =
  let e = Dynarray.fold_left (fun rs x -> val_refs_aux x rs) [] state.e in
  let k = val_refs_aux state.k e in
  k

let rec resolve (w : world) (src : source) : Word.t * seq =
  set_value w.resolved src true;
  let v = get_value w.state src in
  let vt, vh = Generic.front_exn ~monoid ~measure v in
  match vh with
  | Words vh ->
      let vht, vhh = Generic.front_exn ~monoid:Words.monoid ~measure:Words.measure vh in
      let vt = if Generic.is_empty vht then vt else Value.value_cons (Words vht) vt in
      (vhh, vt)
  | _ -> failwith "cannot resolve reference"

let pc_map : exp Dynarray.t = Dynarray.create ()
let reset () = Dynarray.clear pc_map

let add_exp (f : world -> unit) (pc_ : int) : unit =
  let pc = Dynarray.length pc_map in
  assert (Int.equal pc pc_);
  Dynarray.add_last pc_map { step = f; pc }

let pc_to_exp (Pc pc) : exp = Dynarray.get pc_map pc
let from_constructor (ctag : int) : seq = Generic.singleton (Words (Generic.singleton (Word.ConstructorTag ctag)))
let from_int (i : int) : seq = Generic.singleton (Words (Generic.singleton (Word.Int i)))

let to_word (s : seq) : Word.t =
  assert ((Generic.measure ~monoid ~measure s).degree = 1);
  assert ((Generic.measure ~monoid ~measure s).max_degree = 1);
  assert (Generic.size s = 1);
  match Generic.head_exn s with
  | Words w ->
      let wh, wt = Words.words_front_exn w in
      assert (Generic.is_empty wt);
      wh
  | Reference _ -> failwith "conveting reference to_int"

let append (x : seq) (y : seq) : seq = Value.append x y
let appends (x : seq list) : seq = List.fold_right x ~init:empty ~f:append
let pop (s : seq) = pop_n s 1

let rec splits (x : seq) : seq list =
  if is_empty x then []
  else
    let h, t = pop x in
    h :: splits t

let rec splits_1 x =
  let h, _ = pop x in
  h

let rec splits_2 x =
  let h, t = pop x in
  let h2, _ = pop t in
  (h, h2)

let rec splits_3 x =
  let h, t = pop x in
  let h2, t2 = pop t in
  let h3, _ = pop t2 in
  (h, h2, h3)

let rec splits_4 x =
  let h, t = pop x in
  let h2, t2 = pop t in
  let h3, t3 = pop t2 in
  let h4, _ = pop t3 in
  (h, h2, h3, h4)

let list_match (x : seq) : (Word.t * seq) option =
  match Generic.front ~monoid ~measure x with
  | None -> None
  | Some (rest, Words w) ->
      let wh, wt = Words.words_front_exn w in
      Some (wh, if Words.is_empty wt then rest else Generic.cons ~monoid ~measure rest (Words wt))
  | Some (rest, Reference r) -> failwith "list_match on Reference"

let push_env (w : world) (v : value) : unit =
  assert ((Generic.measure ~monoid ~measure v).degree = 1);
  assert ((Generic.measure ~monoid ~measure v).max_degree = 1);
  Dynarray.add_last w.state.e v

let pop_env (w : world) : value =
  let v = Dynarray.pop_last w.state.e in
  assert ((Generic.measure ~monoid ~measure v).degree = 1);
  assert ((Generic.measure ~monoid ~measure v).max_degree = 1);
  v

let env_call (w : world) (keep : int list) (nargs : int) : seq =
  let l = Dynarray.length w.state.e in
  let ret = appends (List.map keep ~f:(fun i -> Dynarray.get w.state.e i)) in
  w.state.e <- Dynarray.init nargs (fun i -> Dynarray.get w.state.e (l - nargs + i));
  assert ((Generic.measure ~monoid ~measure ret).degree = List.length keep);
  assert ((Generic.measure ~monoid ~measure ret).max_degree = List.length keep);
  ret

let restore_env (w : world) (n : int) (seqs : seq) : unit =
  let splitted = List.rev (List.tl_exn (List.rev (splits seqs))) in
  assert (List.length splitted = n);
  assert (Dynarray.length w.state.e = 1);
  let last = Dynarray.get_last w.state.e in
  w.state.e <- Dynarray.of_list splitted;
  Dynarray.add_last w.state.e last

let get_next_cont (seqs : seq) : seq =
  let splitted = splits seqs in
  List.hd_exn (List.rev splitted)

let return_n (w : world) (n : int) (return_exp : exp) : unit =
  assert (Dynarray.length w.state.e = n);
  w.state.e <- Dynarray.of_list [ Dynarray.get_last w.state.e ];
  w.state.c <- return_exp

let drop_n (w : world) (e : int) (n : int) : unit =
  assert (Dynarray.length w.state.e = e);
  let last = Dynarray.pop_last w.state.e in
  let rec loop x =
    if x = n then ()
    else (
      Dynarray.remove_last w.state.e;
      loop (x + 1))
  in
  loop 0;
  Dynarray.add_last w.state.e last

let assert_env_length (w : world) (e : int) : unit =
  let l = Dynarray.length w.state.e in
  if l <> e then print_endline ("env_length should be " ^ string_of_int e ^ " but is " ^ string_of_int l);
  assert (l = e)

let init_memo () : memo = Array.create ~len:(Dynarray.length pc_map) None

let rec value_hash (r : Read.read) (v : Value.value) (hash_acc : int) (value_acc : Value.value) :
    (int * Value.value) option =
  (*assert (Read.read_valid r);
  assert (Value.value_valid v);*)
  if Generic.is_empty r then Some (hash_acc, value_acc)
  else
    let rh, rt = Read.read_front_exn r in
    match rh with
    | RSkip n ->
        let vh, vt = Value.pop_n v n in
        value_hash rt vt hash_acc (Value.append value_acc vh)
    | RRead n ->
        let rec loop n v hash_acc value_acc =
          if n = 0 then value_hash rt v hash_acc value_acc
          else
            let Words w, _ = Value.front_exn v in
            let wh, wt = Words.words_front_exn w in
            let vh, vt = Value.pop_n v 1 in
            let hash_acc = Read.hash hash_acc (Word.hash wh) in
            loop (n - 1) vt hash_acc (Value.append value_acc vh)
        in
        loop n v hash_acc value_acc
    | RCon c -> ( match Value.unwords v c with None -> None | Some v -> value_hash rt v hash_acc value_acc)

let string_of_red (r : Read.red) : string =
  match r with
  | RRead n -> "RRead(" ^ string_of_int n ^ ")"
  | RSkip n -> "RSkip(" ^ string_of_int n ^ ")"
  | RCon w -> "RCon(" ^ Words.string_of_words w ^ ")"

let string_of_read (r : Read.read) : string =
  "[" ^ String.concat ~sep:";" (List.map ~f:string_of_red (Generic.to_list r)) ^ "]"

let string_of_reads (r : reads) : string =
  let k_str = "k: " ^ string_of_read r.k in
  let e_str = "e: [" ^ String.concat ~sep:"; " (List.map ~f:string_of_read (Dynarray.to_list r.e)) ^ "]" in
  "{" ^ e_str ^ k_str ^ "; " ^ "}"

let values_hash_slot = Profile.register_slot Profile.memo_profile "values_hash"

let values_hash_aux (r : reads) (v : State.state) : (int * State.state) option =
  assert (Dynarray.length r.e = Dynarray.length v.e);
  let acc = value_hash r.k v.k 0 Generic.empty in
  let ret result =
    (*(match result with
    | None ->
        print_endline
          ("calling value_hash on\n" ^ string_of_reads r ^ " and state\n" ^ string_of_cek v ^ " returned None")
    | Some (_, rest) ->
        print_endline
          ("calling value_hash on\n" ^ string_of_reads r ^ " and state\n" ^ string_of_cek v
         ^ " returned Some with rest\n" ^ string_of_cek rest));*)
    result
  in
  match acc with
  | None -> ret None
  | Some (k_hash, k_rest) ->
      let e = Dynarray.create () in
      Dynarray.set_capacity e (Dynarray.length v.e);
      let rec loop i j acc =
        if i < j then (
          let r_e = Dynarray.get r.e i in
          let v_e = Dynarray.get v.e i in
          match value_hash r_e v_e acc Generic.empty with
          | None -> ret None
          | Some (r_e_hash, r_e_rest) ->
              Dynarray.add_last e r_e_rest;
              loop (i + 1) j r_e_hash)
        else ret (Some (acc, { c = v.c; e; k = k_rest }))
      in
      loop 0 (Dynarray.length r.e) k_hash

let values_hash (r : reads) (v : State.state) : (int * State.state) option =
  Profile.with_slot values_hash_slot (fun () -> values_hash_aux r v)

let rec read_hash (r : Read.read) (x : Read.read) (hash_acc : int) (read_acc : Read.read) : (int * Read.read) option =
  (*assert (Read.read_valid r);
  assert (Read.read_valid x);*)
  let return ret = ret in
  if Generic.is_empty r then (
    assert (Generic.is_empty x);
    return (Some (hash_acc, read_acc)))
  else
    let rh, rt = Read.read_front_exn r in
    match rh with
    | RSkip n ->
        let xh, xt = Read.read_slice x n in
        return (read_hash rt xt hash_acc (Read.read_append read_acc xh))
    | RRead n ->
        let rec loop n x hash_acc read_acc =
          if n = 0 then return (read_hash rt x hash_acc read_acc)
          else
            let xr, _ = Read.read_front_exn x in
            match xr with
            | RCon con ->
                let w, _ = Words.words_front_exn con in
                let xh, xt = Read.read_slice x 1 in
                let hash_acc = Read.hash hash_acc (Word.hash w) in
                loop (n - 1) xt hash_acc (Read.read_append read_acc xh)
            | RRead _ | RSkip _ -> return None
        in
        loop n x hash_acc read_acc
    | RCon c -> (
        let xh, xt = Read.read_front_exn x in
        match xh with
        | RCon con -> (
            match Words.unwords con c with
            | None -> return None
            | Some xrest ->
                let x = if Generic.is_empty xrest then xt else Read.read_cons (RCon xrest) xt in
                return (read_hash rt x hash_acc read_acc))
        | RRead _ | RSkip _ -> return None)

let reads_equal (x : reads) (y : reads) : bool =
  x.c.pc = y.c.pc && List.equal Read.read_equal (Dynarray.to_list x.e) (Dynarray.to_list y.e) && Read.read_equal x.k y.k

let unmatch_reads (x : reads) (y : reads) : reads = zipwith_ek Read.unmatch_read x y

let reads_hash_aux (r : reads) (x : reads) : (int * reads) option =
  assert (Dynarray.length r.e = Dynarray.length x.e);
  let ret re =
    re
    (*match re with
    | None -> None
    | Some ((_, re) as full_re) ->
        assert (reads_equal x (unmatch_reads r re));
        Some full_re*)
  in
  let acc = read_hash r.k x.k 0 Generic.empty in
  match acc with
  | None -> ret None
  | Some (k_hash, k_rest) ->
      let e = Dynarray.create () in
      Dynarray.set_capacity e (Dynarray.length r.e);
      let rec loop i j acc =
        if i < j then (
          let r_e = Dynarray.get r.e i in
          let x_e = Dynarray.get x.e i in
          match read_hash r_e x_e acc Generic.empty with
          | None -> None
          | Some (r_e_hash, r_e_rest) ->
              Dynarray.add_last e r_e_rest;
              loop (i + 1) j r_e_hash)
        else ret (Some (acc, { c = x.c; e; k = k_rest }))
      in
      loop 0 (Dynarray.length r.e) k_hash

let reads_hash_slot = Profile.register_slot Profile.memo_profile "reads_hash"

let reads_hash (r : reads) (x : reads) : (int * reads) option =
  Profile.with_slot reads_hash_slot (fun () -> reads_hash_aux r x)

let rec match_read (r : Read.read) (x : Read.read) (read_acc : Read.read) : Read.read option =
  (*assert (Read.read_valid r);
  assert (Read.read_valid x);*)
  let return ret = ret in
  if Generic.is_empty r then (
    assert (Generic.is_empty x);
    return (Some read_acc))
  else
    let rh, rt = Read.read_front_exn r in
    match rh with
    | RSkip n ->
        let xh, xt = Read.read_slice x n in
        return (match_read rt xt (Read.read_append read_acc xh))
    | RRead n ->
        let rec loop n x read_acc =
          if n = 0 then return (match_read rt x read_acc)
          else
            let xr, _ = Read.read_front_exn x in
            match xr with
            | RCon con ->
                let xh, xt = Read.read_slice x 1 in
                loop (n - 1) xt (Read.read_append read_acc xh)
            | RRead _ | RSkip _ -> return None
        in
        loop n x read_acc
    | RCon c -> (
        let xh, xt = Read.read_front_exn x in
        match xh with
        | RCon con -> (
            match Words.unwords con c with
            | None -> return None
            | Some xrest ->
                let x = if Generic.is_empty xrest then xt else Read.read_cons (RCon xrest) xt in
                return (match_read rt x read_acc))
        | RRead _ | RSkip _ -> return None)

let match_reads_aux (r : reads) (x : reads) : reads option =
  assert (Dynarray.length r.e = Dynarray.length x.e);
  let ret re =
    re
    (*match re with
    | None -> None
    | Some ((_, re) as full_re) ->
        assert (reads_equal x (unmatch_reads r re));
        Some full_re*)
  in
  let acc = match_read r.k x.k Generic.empty in
  match acc with
  | None -> ret None
  | Some k_rest ->
      let e = Dynarray.create () in
      Dynarray.set_capacity e (Dynarray.length r.e);
      let rec loop i j =
        if i < j then (
          let r_e = Dynarray.get r.e i in
          let x_e = Dynarray.get x.e i in
          match match_read r_e x_e Generic.empty with
          | None -> None
          | Some r_e_rest ->
              Dynarray.add_last e r_e_rest;
              loop (i + 1) j)
        else ret (Some { c = x.c; e; k = k_rest })
      in
      loop 0 (Dynarray.length r.e)

let match_reads_slot = Profile.register_slot Profile.memo_profile "match_reads"

let match_reads (r : reads) (x : reads) : reads option =
  Profile.with_slot match_reads_slot (fun () -> match_reads_aux r x)

let join_reads_slot = Profile.register_slot Profile.memo_profile "join_reads"

type join_reads = { reads : reads Lazy.t; x_weaken : bool; y_weaken : bool }

let join_reads (x : reads) (y : reads) : join_reads =
  Profile.with_slot join_reads_slot (fun () ->
      (*print_endline "calling joining reads:";*)
      let x_weaken = ref false in
      let y_weaken = ref false in
      let join a b =
        let ret = Read.join a x_weaken b y_weaken (lazy Generic.empty) in
        (*print_endline ("join reads:\n  " ^ string_of_read a ^ "\n  " ^ string_of_read b ^ "\n= " ^ string_of_read ret);*)
        ret
      in
      let reads = zipwith_ek join x y in
      let ret = { reads = lazy (map_ek Lazy.force reads); x_weaken = !x_weaken; y_weaken = !y_weaken } in
      (*assert (reads_equal x (unmatch_reads ret.reads ret.x_rest));
      assert (reads_equal y (unmatch_reads ret.reads ret.y_rest));*)
      ret)

let reads_from_patterns (p : Pattern.pattern cek) : reads = map_ek Read.read_from_pattern p
let string_of_trie (t : trie) : string = match t with Leaf _ -> "Leaf" | Branch _ -> "Branch"

let patterns_of_state (x : state) : Pattern.pattern cek =
  let rec f (v : Value.value) =
    if Generic.is_empty v then Generic.empty
    else
      let vt, vh = Generic.front_exn ~monoid ~measure v in
      match vh with Words w -> Pattern.pattern_cons (Pattern.PCon w) (f vt) | Reference r -> failwith "impossible"
  in
  map_ek f x

let rec insert (x : trie) (step : step) (before : state) (after : state) : trie =
  match x with
  | Leaf x -> (
      let j = join_reads (reads_from_patterns x.step.src) (reads_from_patterns (patterns_of_state before)) in
      match (j.x_weaken, j.y_weaken) with
      | false, false ->
          if x.step.sc > step.sc then Leaf x
          else Leaf { step = { step with src = patterns_of_state before; dst = after } }
      | x, y -> failwith ("insert leaf: unimplemented: " ^ string_of_bool x ^ ", " ^ string_of_bool y))
  | Branch _ -> failwith "insert branch: unimplemented"

and insert_option (x : trie option) (step : step) (before : state) (after : state) : trie =
  match x with
  | None -> Leaf { step = { step with src = patterns_of_state before; dst = after } }
  | Some x -> insert x step before after

let insert_step (m : memo) (step : step) (before : state) (after : state) : unit =
  let start_time = Time_stamp_counter.now () in
  Array.set m step.src.c.pc (Some (insert_option (Array.get m step.src.c.pc) step before after));
  let end_time = Time_stamp_counter.now () in
  let calibrator = Lazy.force Time_stamp_counter.calibrator in
  let elapsed_time =
    Time_stamp_counter.diff end_time start_time
    |> Time_stamp_counter.Span.to_time_ns_span ~calibrator
    |> Core.Time_ns.Span.to_int63_ns |> Core.Int63.to_int_exn
  in
  step.insert_time <- elapsed_time

let rec lookup_step_aux (value : state) (trie : trie) (acc : step option) : step option =
  match trie with
  | Leaf leaf ->
      if Dependency.can_step_through leaf.step value then
        match acc with
        | None -> Some leaf.step
        | Some acc_step -> if leaf.step.sc > acc_step.sc then Some leaf.step else acc
      else acc
  | Branch br -> failwith "lookup_step_aux branch: unimplemented"

let lookup_step (value : state) (m : memo) : step option =
  let pc = value.c.pc in
  match Array.get m pc with None -> None | Some trie -> lookup_step_aux value trie None

type 'a bin = 'a digit list
and 'a digit = Zero | One of 'a

let rec inc (f : 'a -> 'a -> 'a) (x : 'a) (y : 'a bin) : 'a bin =
  match y with [] -> [ One x ] | Zero :: ys -> One x :: ys | One y :: ys -> Zero :: inc f (f x y) ys

let rec fold_bin (f : 'a -> 'a -> 'a) (acc : 'a option) (x : 'a bin) : 'a option =
  match x with
  | [] -> acc
  | Zero :: xs -> fold_bin f acc xs
  | One x :: xs -> ( match acc with Some acc -> fold_bin f (Some (f acc x)) xs | None -> fold_bin f (Some x) xs)

type history = slice bin ref

(* we dont really need state for composition, but it is good for bug catching. *)
and slice = { before : state; after : state; step : step }

let exec_cek_slot = Profile.register_slot Profile.memo_profile "exec_cek"
let step_through_slot = Profile.register_slot Profile.memo_profile "step_through"
let compose_step_slot = Profile.register_slot Profile.memo_profile "compose_step"
let insert_step_slot = Profile.register_slot Profile.memo_profile "insert_step"
let lookup_step_slot = Profile.register_slot Profile.memo_profile "lookup_step"

let exec_cek (c : exp) (e : words Dynarray.t) (k : words) (m : memo) : exec_result =
  let run () =
    let raw_step s =
      let w = make_world (copy_state s) m in
      s.c.step w;
      w.state
    in
    let rec raw_step_n s n = if n = 0 then s else raw_step_n (raw_step s) (n - 1) in
    let dbg_step_through step state =
      assert (step.sc > 0);
      step.hit <- step.hit + 1;
      (*if not (Dependency.can_step_through step state) then
        print_endline ("cannot step through: " ^ Dependency.string_of_step step);*)
      let x = Profile.with_slot step_through_slot (fun () -> Dependency.step_through step state) in
      (*let y = raw_step_n state step.sc in
      if not (Dependency.state_equal x y) then (
        print_endline "state before step:";
        print_endline (string_of_cek state);
        print_endline "state after step:";
        print_endline (string_of_cek x);
        print_endline "expected:";
        print_endline (string_of_cek y));
      assert (Dependency.state_equal x y);*)
      x
    in
    let state = { c; e; k } in
    let i = ref 0 in
    let sc = ref 0 in
    let hist : history = ref [] in
    (* Binary counter that incrementally composes adjacent slices; arguments are
       reversed so the newest slice sits on the right-hand side during carry. *)
    let compose_slice (y : slice) (x : slice) =
      let step = Profile.with_slot compose_step_slot (fun () -> Dependency.compose_step x.step y.step) in
      Profile.with_slot insert_step_slot (fun () -> insert_step m step x.before y.after);
      (*let lookuped = lookup_step x.state m |> Option.value_exn in
      if lookuped != step then
        failwith
          ("composed step is different from looked up step:" ^ Dependency.string_of_step step ^ "lookuped: "
         ^ Dependency.string_of_step lookuped);*)
      { before = x.before; after = y.after; step }
    in
    let rec exec state =
      if is_done state then state
      else (
        (*let _ = map_ek (fun v -> assert (value_valid v)) state in*)
        log ("step " ^ string_of_int !i ^ ": " ^ string_of_int !sc);
        i := !i + 1;
        match Profile.with_slot lookup_step_slot (fun () -> lookup_step state m) with
        | Some step ->
            let after = dbg_step_through step state in
            hist := inc compose_slice { before = state; after; step } !hist;
            sc := !sc + step.sc;
            exec after
        | None ->
            let old = copy_state state in
            let w = make_world state m in
            state.c.step w;
            let step = Dependency.make_step old w.resolved m in
            sc := !sc + step.sc;
            let after = dbg_step_through step old in
            insert_step m step old after;
            hist := inc compose_slice { before = old; after; step } !hist;
            exec after)
    in
    let state = exec state in
    assert (Dynarray.length state.e = 1);
    ignore (fold_bin compose_slice None !hist);
    print_endline ("took " ^ string_of_int !i ^ " step, but without memo take " ^ string_of_int !sc ^ " step.");
    { words = Dynarray.get_last state.e; step = !i; without_memo_step = !sc }
  in
  Gc.full_major ();
  let result = Profile.with_slot exec_cek_slot run in
  result

let exec_cek_raw (c : exp) (e : words Dynarray.t) (k : words) =
  let state = { c; e; k } in
  let m = init_memo () in
  let rec exec state =
    if is_done state then state
    else
      let w = make_world state m in
      state.c.step w;
      exec w.state
  in
  Dynarray.get_last (exec state).e

let exec_done _ = failwith "exec is done, should not call step anymore"
let pattern_size (p : Pattern.pattern) = Generic.size p
let patterns_size (p : Pattern.pattern cek) : int = fold_ek p 0 (fun acc p -> acc + pattern_size p)

type memo_stats = { by_depth : by_depth Dynarray.t; rule_stat : rule_stat list }
and by_depth = { depth : int; mutable node_count : int }
and rule_stat = { size : int; sc : int; hit_count : int; insert_time : int; depth : int; rule : string }

let memo_stats (m : memo) : memo_stats =
  let by_depth = Dynarray.create () in
  let rule_stat = ref [] in
  let rec aux (t : trie) (depth : int) : unit =
    if Dynarray.length by_depth <= depth then Dynarray.add_last by_depth { depth; node_count = 0 };
    let node_stat = Dynarray.get by_depth depth in
    node_stat.node_count <- node_stat.node_count + 1;
    match t with Branch br -> Hashtbl.iter br.children ~f:(fun child -> aux child (depth + 1))
  in
  Array.iter m ~f:(fun opt_trie -> match opt_trie with None -> () | Some trie -> aux trie 0);
  { by_depth; rule_stat = !rule_stat }
