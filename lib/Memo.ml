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
type exec_result = { words : words; step : int; without_memo_step : int; wall_time : int; without_memo_wall_time : int }

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
  let lhs, rhs = Generic.split ~monoid ~measure (fun m -> Option.is_none m.full) x in
  assert (Option.is_some (Generic.measure ~monoid ~measure lhs).full);
  match Generic.front rhs ~monoid ~measure with
  | None -> lhs
  | Some (rest, Reference r) ->
      Generic.append ~monoid ~measure lhs
        (Generic.append ~monoid ~measure (subst_reference resolve r) (subst resolve rest))

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
  let lhs, rhs = Generic.split ~monoid ~measure (fun m -> Option.is_none m.full) x in
  assert (Option.is_some (Generic.measure ~monoid ~measure lhs).full);
  match Generic.front rhs ~monoid ~measure with None -> rs | Some (rest, Reference r) -> val_refs_aux rest (r :: rs)

let state_refs (state : state) : reference list =
  let e = Dynarray.fold_left (fun rs x -> val_refs_aux x rs) [] state.e in
  let k = val_refs_aux state.k e in
  k

let rec resolve (w : world) (src : source) : Word.t * seq =
  set_value w.resolved src true;
  let v = get_value w.state src in
  let vt, vh = Generic.front_exn ~monoid ~measure v in
  match vh with Word vh -> (vh, vt) | _ -> failwith "cannot resolve reference"

let pc_map : exp Dynarray.t = Dynarray.create ()

let add_exp (f : world -> unit) (pc_ : int) : unit =
  let pc = Dynarray.length pc_map in
  assert (Int.equal pc pc_);
  Dynarray.add_last pc_map { step = f; pc }

let pc_to_exp (Pc pc) : exp = Dynarray.get pc_map pc
let from_constructor (ctag : int) : seq = Generic.singleton (Word (Word.ConstructorTag ctag))
let from_int (i : int) : seq = Generic.singleton (Word (Word.Int i))

let to_word (s : seq) : Word.t =
  assert ((Generic.measure ~monoid ~measure s).degree = 1);
  assert ((Generic.measure ~monoid ~measure s).max_degree = 1);
  assert (Generic.size s = 1);
  match Generic.head_exn s with Word w -> w | Reference _ -> failwith "conveting reference to_int"

let append (x : seq) (y : seq) : seq = Generic.append ~monoid ~measure x y
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
  Option.map (Generic.front ~monoid ~measure x) ~f:(fun (x, Word y) -> (y, x))

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

let rec value_hash (r : Read.read) (v : Value.value) (hashacc : int) : int option =
  if Generic.is_empty r then Some hashacc
  else
    let rh, rt = Read.read_front_exn r in
    match rh with
    | RSkip n ->
        let _, v = Value.pop_n v n in
        value_hash rt v hashacc
    | RRead n ->
        let rec loop n v hashacc =
          if n = 0 then value_hash rt v hashacc
          else
            let Word w, _ = Value.front_exn v in
            let _, v = Value.pop_n v 1 in
            let hashacc = Read.hash hashacc (Word.hash w) in
            loop (n - 1) v hashacc
        in
        loop n v hashacc
    | RCon c -> ( match Value.unwords v c with None -> None | Some v -> value_hash rt v hashacc)

let values_hash (r : reads) (v : State.state) : int option =
  let acc = Some 0 in
  let acc = Option.bind acc (fun acc -> value_hash r.k v.k acc) in
  assert (Dynarray.length r.e = Dynarray.length v.e);
  let rec loop i j acc =
    if i < j then
      let r_e = Dynarray.get r.e i in
      let v_e = Dynarray.get v.e i in
      let acc = Option.bind acc (fun acc -> value_hash r_e v_e acc) in
      loop (i + 1) j acc
    else acc
  in
  loop 0 (Dynarray.length r.e) acc

let rec pattern_hash (r : Read.read) (p : Pattern.pattern) (hashacc : int) : int option =
  if Generic.is_empty r then Some hashacc
  else
    let rh, rt = Read.read_front_exn r in
    match rh with
    | RSkip n ->
        let _, p = Pattern.pattern_slice p n in
        pattern_hash rt p hashacc
    | RRead n ->
        let rec loop n p hashacc =
          if n = 0 then pattern_hash rt p hashacc
          else
            let pat, _ = Pattern.pattern_front_exn p in
            match pat with
            | PCon con ->
                let w, _ = Words.words_front_exn con in
                let _, p = Pattern.pattern_slice p 1 in
                loop (n - 1) p (Read.hash hashacc (Word.hash w))
            | PVar _ -> None
        in
        loop n p hashacc
    | RCon c -> (
        let ph, pt = Pattern.pattern_front_exn p in
        match ph with
        | PCon con -> (
            match Words.unwords con c with
            | None -> None
            | Some prest ->
                let p = if Generic.is_empty prest then pt else Pattern.pattern_cons (PCon prest) pt in
                pattern_hash rt p hashacc)
        | PVar _ -> None)

let patterns_hash (r : reads) (p : Pattern.pattern cek) : int option =
  let acc = Some 0 in
  let acc = Option.bind acc (fun acc -> pattern_hash r.k p.k acc) in
  assert (Dynarray.length r.e = Dynarray.length p.e);
  let rec loop i j acc =
    if i < j then
      let r_e = Dynarray.get r.e i in
      let p_e = Dynarray.get p.e i in
      let acc = Option.bind acc (fun acc -> pattern_hash r_e p_e acc) in
      loop (i + 1) j acc
    else acc
  in
  loop 0 (Dynarray.length r.e) acc

let rec read_hash (r : Read.read) (x : Read.read) (hashacc : int) : int option =
  if Generic.is_empty r then Some hashacc
  else
    let rh, rt = Read.read_front_exn r in
    match rh with
    | RSkip n ->
        let x = Read.read_pop_n x n in
        read_hash rt x hashacc
    | RRead n ->
        let rec loop n x hashacc =
          if n = 0 then read_hash rt x hashacc
          else
            let xr, x = Read.read_front_exn x in
            match xr with
            | RCon con ->
                let w, _ = Words.words_front_exn con in
                let x = Read.read_pop_n x 1 in
                let hashacc = Read.hash hashacc (Word.hash w) in
                loop (n - 1) x hashacc
            | RRead _ | RSkip _ -> None
        in
        loop n x hashacc
    | RCon c -> (
        let xh, xt = Read.read_front_exn x in
        match xh with
        | RCon con -> (
            match Words.unwords con c with
            | None -> None
            | Some xrest ->
                let x = if Generic.is_empty xrest then xt else Read.read_cons (RCon xrest) xt in
                read_hash rt x hashacc)
        | RRead _ | RSkip _ -> None)

let reads_hash (r : reads) (x : reads) : int option =
  let acc = Some 0 in
  let acc = Option.bind acc (fun acc -> read_hash r.k x.k acc) in
  assert (Dynarray.length r.e = Dynarray.length x.e);
  let rec loop i j acc =
    if i < j then
      let r_e = Dynarray.get r.e i in
      let x_e = Dynarray.get x.e i in
      let acc = Option.bind acc (fun acc -> read_hash r_e x_e acc) in
      loop (i + 1) j acc
    else acc
  in
  loop 0 (Dynarray.length r.e) acc

let rec lookup_step_aux (value : state) (trie : trie) (acc : step option) : step option =
  match trie with
  | Atom step ->
      if Dependency.can_step_through step value then
        match acc with None -> Some step | Some step' -> if step.sc > step'.sc then Some step else Some step'
      else acc
  | Subsume (parent, children) ->
      if Dependency.can_step_through parent value then
        let acc =
          match acc with None -> Some parent | Some step' -> if parent.sc > step'.sc then Some parent else Some step'
        in
        lookup_step_aux value children acc
      else acc
  | Split { reads; children } -> (
      match values_hash reads value with
      | None -> acc
      | Some key -> (
          match Hashtbl.find children key with None -> acc | Some child_trie -> lookup_step_aux value child_trie acc))

let lookup_step (value : state) (m : memo) : step option =
  let pc = value.c.pc in
  match Array.get m pc with None -> None | Some trie -> lookup_step_aux value trie None

let string_of_red (r : Read.red) : string =
  match r with
  | RRead n -> "RRead(" ^ string_of_int n ^ ")"
  | RSkip n -> "RSkip(" ^ string_of_int n ^ ")"
  | RCon w -> "RCon(" ^ Dependency.string_of_words w ^ ")"

let string_of_read (r : Read.read) : string =
  "[" ^ String.concat ~sep:";" (List.map ~f:string_of_red (Generic.to_list r)) ^ "]"

type join_reads = { reads : reads; x_weaken : bool; y_weaken : bool }

let join_reads (x : reads) (y : reads) : join_reads =
  (*print_endline "calling joining reads:";*)
  let x_weaken = ref false in
  let y_weaken = ref false in
  let join (a, b) =
    let ret = Read.join a b x_weaken y_weaken in
    (*print_endline ("join reads:\n  " ^ string_of_read a ^ "\n  " ^ string_of_read b ^ "\n= " ^ string_of_read ret);*)
    ret
  in
  let reads = zip_ek x y |> Option.value_exn |> map_ek join in
  { reads; x_weaken = !x_weaken; y_weaken = !y_weaken }

let reads_from_patterns (p : Pattern.pattern cek) : reads = map_ek Read.read_from_pattern p
let string_of_trie (t : trie) : string = match t with Atom _ -> "Atom" | Subsume _ -> "Subsume" | Split _ -> "Split"

let reads_from_trie (t : trie) : reads =
  match t with
  | Atom s -> reads_from_patterns s.src
  | Subsume (s, _) -> reads_from_patterns s.src
  | Split { reads; _ } -> reads

let rec merge (x : trie) (y : trie) : trie =
  match (x, y) with
  | Atom x, Atom y -> (
      let j = join_reads (reads_from_patterns x.src) (reads_from_patterns y.src) in
      match (j.x_weaken, j.y_weaken) with
      | false, true -> Subsume (x, Atom y)
      | true, false -> Subsume (y, Atom x)
      | false, false -> if x.sc >= y.sc then Atom x else Atom y
      | true, true ->
          let children = Hashtbl.create (module Int) in
          let x_key = Option.value_exn (patterns_hash j.reads x.src) in
          let y_key = Option.value_exn (patterns_hash j.reads y.src) in
          assert (x_key <> y_key);
          Hashtbl.set children x_key (Atom x);
          Hashtbl.set children y_key (Atom y);
          Split { reads = j.reads; children })
  | Subsume (xp, xc), Atom y -> (
      let j = join_reads (reads_from_patterns xp.src) (reads_from_patterns y.src) in
      match (j.x_weaken, j.y_weaken) with
      | false, false -> Subsume ((if xp.sc >= y.sc then xp else y), xc)
      | false, true -> Subsume (xp, merge xc (Atom y))
      | true, false -> Subsume (y, Subsume (xp, xc))
      | true, true ->
          let children = Hashtbl.create (module Int) in
          let x_key = Option.value_exn (patterns_hash j.reads xp.src) in
          let y_key = Option.value_exn (patterns_hash j.reads y.src) in
          assert (x_key <> y_key);
          Hashtbl.set children x_key (Subsume (xp, xc));
          Hashtbl.set children y_key (Atom y);
          Split { reads = j.reads; children })
  | Atom x, Subsume (yp, yc) -> merge (Subsume (yp, yc)) (Atom x)
  | Split { reads = xr; children = xc }, Atom y -> (
      let j = join_reads xr (reads_from_patterns y.src) in
      match (j.x_weaken, j.y_weaken) with
      | false, true ->
          let y_key = Option.value_exn (patterns_hash xr y.src) in
          Hashtbl.update xc y_key ~f:(merge_option (Atom y));
          Split { reads = xr; children = xc }
      | true, false -> Subsume (y, Split { reads = xr; children = xc })
      | true, true ->
          print_endline "here";
          let children = Hashtbl.create (module Int) in
          let y_key = Option.value_exn (patterns_hash j.reads y.src) in
          (*Hashtbl.iter xc ~f:(fun x ->
              let x_key = Option.value_exn (reads_hash j.reads (reads_from_trie x)) in
              Hashtbl.update children x_key ~f:(merge_option x));*)
          (*cannot reuse key*)
          (*Hashtbl.iteri xc ~f:(fun ~key ~data -> Hashtbl.update children key ~f:(merge_option data));*)
          Hashtbl.update children y_key ~f:(merge_option (Atom y));
          Split { reads = j.reads; children }
      | _ ->
          failwith
            ("merge not implemented for split/atom:" ^ string_of_bool j.x_weaken ^ "," ^ string_of_bool j.y_weaken))
  | Atom x, Split { reads = yr; children = yc } -> merge (Split { reads = yr; children = yc }) (Atom x)
  | _ -> failwith ("merge not implemented yet for: " ^ string_of_trie x ^ " and " ^ string_of_trie y)

and merge_option (x : trie) (y : trie option) : trie = match y with None -> x | Some y -> merge x y

let insert_step (m : memo) (step : step) : unit =
  Array.set m step.src.c.pc (Some (merge_option (Atom step) (Array.get m step.src.c.pc)))

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
and slice = { state : state; step : step }

let exec_cek (c : exp) (e : words Dynarray.t) (k : words) (m : memo) : exec_result =
  let start_time = Time_stamp_counter.now () in
  let calibrator = Lazy.force Time_stamp_counter.calibrator in
  let raw_step s =
    let w = make_world (copy_state s) m in
    s.c.step w;
    w.state
  in
  let rec raw_step_n s n = if n = 0 then s else raw_step_n (raw_step s) (n - 1) in
  let dbg_step_through step state =
    assert (step.sc > 0);
    let x = Dependency.step_through step state in
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
    let step = Dependency.compose_step x.step y.step in
    insert_step m step;
    { state = x.state; step }
  in
  let rec exec state =
    if is_done state then state
    else (
      log ("step " ^ string_of_int !i ^ ": " ^ string_of_int !sc);
      i := !i + 1;
      match lookup_step state m with
      | Some step ->
          hist := inc compose_slice { state; step } !hist;
          sc := !sc + step.sc;
          dbg_step_through step state |> exec
      | None ->
          let old = copy_state state in
          let w = make_world state m in
          state.c.step w;
          let step = Dependency.make_step old w.resolved m in
          sc := !sc + step.sc;
          insert_step m step;
          hist := inc compose_slice { state = old; step } !hist;
          let st = dbg_step_through step old in
          exec st)
  in
  let state = exec state in
  assert (Dynarray.length state.e = 1);
  ignore (fold_bin compose_slice None !hist);
  let wall_time =
    Time_stamp_counter.diff (Time_stamp_counter.now ()) start_time
    |> Time_stamp_counter.Span.to_time_ns_span ~calibrator
    |> Time_ns.Span.to_int63_ns |> Int63.to_int_exn
  in
  print_endline ("took " ^ string_of_int !i ^ " step, but without memo take " ^ string_of_int !sc ^ " step.");
  {
    words = Dynarray.get_last state.e;
    step = !i;
    without_memo_step = !sc;
    wall_time;
    without_memo_wall_time = wall_time;
  }

let exec_done _ = failwith "exec is done, should not call step anymore"
