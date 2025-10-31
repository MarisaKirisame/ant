(* Required reading: Seq.ml
 *
 * This file contian all code that deals with memoiziation.
 * There is multiple mutually recursive component which together enable memoization.
 * However, three of them are the most critical:
 *   The Reference type. It represent fragment of the finger tree that is unavailable, as we're working on fragment of the tree.
 *   The Store. It give meaning to Reference type in the tree.
 *   The memo. It is a fusion between hashtable and trie, somewhat like the patrica trie, to traverse prefix at exponential rate.
 *)
open BatFingerTree
open Word
module Hasher = Hash.MCRC32C
open Base

(*module Hasher = Hash.MCRC32*)
(*module Hasher = Hash.SL2*)
(*module Hasher = Hash.DebugHash*)
module Hashtbl = Core.Hashtbl
open Value
open State
open Common

let log x = print_endline x
let log x = ignore x

(* todo: when the full suffix is fetched, try to extend at front. *)
type fetch_result = { fetched : words; prefix : reference option; suffix : reference option }
and words = seq
(* Just have Word.t. We could make Word a finger tree of Word.t but that would cost lots of conversion between two representation. *)

let source_to_string (src : source) =
  match src with E i -> "E" ^ string_of_int i | S i -> "S" ^ string_of_int i | K -> "K"

let request_to_string (req : fetch_request) =
  "at " ^ source_to_string req.src ^ "+" ^ string_of_int req.offset ^ ", " ^ string_of_int req.word_count ^ " words"

let fr_to_fh (fr : fetch_result) : fetch_hash =
  let ret = Hasher.hash (Option.value_exn (Generic.measure ~measure ~monoid fr.fetched).full).hash in
  ret

let get_value (state : state) (store : store option) (src : source) : value =
  match src with
  | E i ->
      assert (i < Dynarray.length state.e);
      Dynarray.get state.e i
  | S i ->
      assert (i < Dynarray.length (Option.value_exn store));
      Dynarray.get (Option.value_exn store) i
  | K -> state.k

(* Where does the references in tstate point to? 
 * Note that references in Here can only point to S.
 *)
type tvalue = Here of seq | Lower of seq
type tstore = tvalue Dynarray.t
type tstate = { mutable tc : exp; mutable te : tvalue Dynarray.t; mutable tk : tvalue; mutable ts : tstore }

let set_tvalue (ts : tstate) (src : source) (tv : tvalue) : unit =
  match src with E i -> Dynarray.set ts.te i tv | S i -> Dynarray.set ts.ts i tv | K -> ts.tk <- tv

let init_store () : store = Dynarray.create ()

let add_to_store (store : store) (tstore : tstore) (v : value) : reference =
  let d = (Generic.measure ~monoid ~measure v).degree in
  (* if it is 0 it must be empty and we should not be adding *)
  assert (d > 0);
  let r = { src = S (Dynarray.length store); offset = 0; values_count = d } in
  Dynarray.add_last store v;
  Dynarray.add_last tstore (Lower (Generic.singleton (Reference r)));
  r

let seq_of_fetch_result (fr : fetch_result) : seq =
  let words = fr.fetched in
  let with_prefix =
    match fr.prefix with
    | None -> words
    | Some r -> Generic.append ~monoid ~measure (Generic.singleton (Reference r)) words
  in
  let with_suffix =
    match fr.suffix with
    | None -> with_prefix
    | Some r -> Generic.append ~monoid ~measure with_prefix (Generic.singleton (Reference r))
  in
  with_suffix

let fetch_value (state : state) (store : store) (tstate : tstate) (req : fetch_request) : fetch_result option =
  let v = get_value state (Some store) req.src in
  let x, y = pop_n v req.offset in
  let words, rest =
    Generic.split ~monoid ~measure
      (fun m -> not (match m.full with Some f -> f.length <= req.word_count | None -> false))
      y
  in
  let m = Generic.measure ~monoid ~measure words in
  let length = (Option.value_exn m.full).length in
  assert (length <= req.word_count);
  if (not (Generic.is_empty rest)) && length != req.word_count then
    (* We could try to return the shorten fragment and continue.
     * however i doubt it is reusable so we are just cluttering the hashtable*)
    None
  else
    let have_prefix = not (Generic.is_empty x) in
    let have_suffix = not (Generic.is_empty rest) in
    let prefix = if have_prefix then add_to_store store tstate.ts x |> Some else None in
    let suffix = if have_suffix then add_to_store store tstate.ts rest |> Some else None in
    let fr = { fetched = words; prefix; suffix } in
    set_tvalue tstate req.src (Here (seq_of_fetch_result fr));
    Some fr

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
  let sc = x.sc in
  { c; e; k; sc }

(* fast forward state x to state y via store *)
let fast_forward_to (x : state) (store : store) (y : state) : state =
  let state = subst_state y (Option.some % get_value x (Some store)) in
  let sc = state.sc + x.sc in
  { c = state.c; e = state.e; k = state.k; sc }

(* Stepping require an unfetched fragment. register the current state. *)
let register_memo_need_unfetched (state : state) (request : fetch_request) (update : update) : unit =
  match !update with
  | BlackHole | Halfway _ ->
      let lookup = Hashtbl.create (module Core.Int) in
      log ("updating need with: " ^ string_of_cek state);
      update := Need { next = { request; lookup }; current = Shared state }
  | Need _ -> ()
  | Done _ -> failwith "Done canot be fetching"

let single_reference (src : source) : seq = Generic.singleton (Reference { src; offset = 0; values_count = 1 })
let print_state (cek : state) msg : unit = print_endline (msg ^ ": pc=" ^ string_of_int cek.c.pc)

(* Each execution cycle in ant consist of 3 steps:
 * - Locating a node from the memo tree, which consist of an updatable intermediate state.
 * - Updating that state in the memo tree by moving forward.
 * - Forwarding to that state.
 * These 3 steps use and improve the memo tree at the same time.
 *)

let tstate_from_state (s : state) : tstate =
  {
    tc = s.c;
    te = Dynarray.mapi (fun i _ -> Lower (single_reference (E i))) s.e;
    tk = Lower (single_reference K);
    ts = Dynarray.create ();
  }

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

let unHere (Here seq) = seq
let unLower (Lower seq) = seq

let state_from_tstate (s : state) (t : tstate) : state =
  dyn_array_rev_update
    (fun v ->
      match v with
      | Here v -> Lower (subst (fun (S i) -> Option.some $ unLower (Dynarray.get t.ts i)) v)
      | Lower v -> Lower v)
    t.ts;
  let transform (tv : tvalue) : value =
    match tv with Here v -> subst (fun (S i) -> Option.some $ unLower (Dynarray.get t.ts i)) v | Lower v -> v
  in
  let ret = { c = t.tc; e = Dynarray.map transform t.te; k = transform t.tk; sc = 0 } in
  log "state_from_tstate ";
  ret

let rec val_refs_aux (x : value) (rs : reference list) : reference list =
  let lhs, rhs = Generic.split ~monoid ~measure (fun m -> Option.is_none m.full) x in
  assert (Option.is_some (Generic.measure ~monoid ~measure lhs).full);
  match Generic.front rhs ~monoid ~measure with None -> rs | Some (rest, Reference r) -> val_refs_aux rest (r :: rs)

let state_refs (state : state) : reference list =
  let e = Dynarray.fold_left (fun rs x -> val_refs_aux x rs) [] state.e in
  let k = val_refs_aux state.k e in
  k

exception DoneExec of state

let rec climb_aux (state : state) (store : store) (tstate : tstate) (fetch_value_can_fail : bool)
    (climb_done : state -> 'a) (climb_halfway : state -> update -> 'a) (update : update) (depth : int) : 'a =
  match !update with
  | Halfway s ->
      log ("climb_halfway depth: " ^ string_of_int depth);
      climb_halfway (copy_state s) update
  | Done d -> climb_done (copy_state d)
  (* This is the only place where tstate is used. Maybe just remove the idea of blackhole? *)
  | BlackHole -> climb_halfway (state_from_tstate state tstate) update
  | Need { current; next } -> (
      match fetch_value state store tstate next.request with
      | None -> climb_halfway (copy_state current) update
      | Some fr -> (
          match Hashtbl.find next.lookup (fr_to_fh fr) with
          | None ->
              let bh = ref BlackHole in
              Hashtbl.add_exn next.lookup ~key:(fr_to_fh fr) ~data:bh;
              let state =
                subst_state (copy_state current) (fun s ->
                    if Source.( = ) s next.request.src then Option.some $ seq_of_fetch_result fr else None)
              in
              climb_halfway state bh
          | Some m -> climb_aux state store tstate fetch_value_can_fail climb_done climb_halfway m (depth + 1)))

let rec climb (state : state) (store : store) (fetch_value_can_fail : bool) (climb_done : state -> 'a)
    (climb_halfway : state -> update -> 'a) (memo : memo_t) : 'a =
  let tstate = tstate_from_state state in
  climb_aux state store tstate fetch_value_can_fail climb_done climb_halfway (Array.get memo state.c.pc) 0

(* find a node on the tree as far as possible. *)
let locate (state : state) (memo : memo_t) : state * store * update =
  let store = init_store () in
  climb state store false
    (fun d -> raise (DoneExec (fast_forward_to state store d)))
    (fun state update -> (state, store, update))
    memo

let improve update u =
  (match (!update, u) with
  | Need _, _ -> failwith "impossible to improve Need"
  | Done _, _ -> failwith "impossible to improve Done"
  | _, BlackHole -> failwith "blakchole does not improve"
  | BlackHole, _ | _, Need _ | _, Done _ -> ()
  | Halfway (Shared l), Halfway (Shared r) -> assert (l.sc < r.sc));
  (match u with
  | BlackHole -> ()
  | Need { current = Shared x; _ } | Done (Shared x) | Halfway (Shared x) -> log "improving with: ");
  update := u

let update (state : state) (store : store) (memo : memo_t) (update : update) : state =
  let sc_before = state.sc in
  let state =
    let store = init_store () in
    climb state store true
      (fun d ->
        let d = fast_forward_to state store d in
        improve update (Done (Shared d));
        raise (DoneExec d))
      (fun y _ ->
        log "update of state:";
        fast_forward_to state store y)
      memo
  in
  assert (sc_before <= state.sc);
  if sc_before == state.sc then (
    log ("before step taken:sc=" ^ string_of_int state.sc ^ ", pc=" ^ string_of_int state.c.pc);
    ignore (state.c.step (make_world state store update));
    log ("after step taken:sc=" ^ string_of_int state.sc ^ ", pc=" ^ string_of_int state.c.pc));
  if sc_before < state.sc then improve update (Halfway (Shared state));
  state

(* The point of the ant_step function is to both improve the memo tree, creating larger and larger skip,
 * as well as actually moving forward along the tree as well.
 *)
let ant_step (x : state) (m : memo_t) : state =
  let y, store, update_ = locate x m in
  log "located a state: ";
  let y = try update y store m update_ with DoneExec d -> raise (DoneExec (fast_forward_to x store d)) in
  fast_forward_to x store y

let pow2_int x = 1 lsl x
let get_word_count (store : store) : int = pow2_int $ Dynarray.length store

let rec resolve_seq (w : world) (x : seq) : (Word.t * seq) option =
  let m = Generic.measure ~monoid ~measure x in
  assert (m.degree = 1);
  assert (m.max_degree = 1);
  let tl, hd = Generic.front_exn ~monoid ~measure x in
  match hd with
  | Word w -> Some (w, tl)
  | Reference ref ->
      register_memo_need_unfetched w.state
        { src = ref.src; offset = ref.offset; word_count = get_word_count w.store }
        w.update;
      None

(* Src cannot be a Store, as we are resolving location at the top level, while only non-top-level have store. *)
let rec resolve (w : world) (src : source) : (Word.t * seq) option =
  (* Note that we deliberately not pass store into get_value. 
   * The store is only used for fetch length calculation, and the actual content is discarded.
   *)
  let v = get_value w.state None src in
  assert ((Generic.measure ~monoid ~measure v).degree = 1);
  assert ((Generic.measure ~monoid ~measure v).max_degree = 1);
  resolve_seq w v

let pc_map : exp Dynarray.t = Dynarray.create ()

let add_exp (f : world -> unit) (pc_ : int) : unit =
  let pc = Dynarray.length pc_map in
  assert (pc == pc_);
  Dynarray.add_last pc_map { step = f; pc }

let pc_to_exp (Pc pc) : exp = Dynarray.get pc_map pc
let from_constructor (ctag : int) : seq = Generic.singleton (Word (Word.make Word.constructor_tag ctag))
let from_int (i : int) : seq = Generic.singleton (Word (Word.make Word.int_tag i))

let to_int (s : seq) : int =
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

let list_match (x : seq) : (Word.t * seq) option =
  Option.map (Generic.front ~monoid ~measure x) (fun (x, Word y) -> (y, x))

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
  let ret = appends (List.map keep (fun i -> Dynarray.get w.state.e i)) in
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

let stepped (w : world) : unit = w.state.sc <- w.state.sc + 1

let return_n (w : world) (n : int) (return_exp : exp) : unit =
  assert (Dynarray.length w.state.e = n);
  w.state.e <- Dynarray.of_list [ Dynarray.get_last w.state.e ];
  w.state.c <- return_exp;
  stepped w

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

let exec_done (w : world) : unit =
  improve w.update (Done (Shared w.state));
  raise (DoneExec w.state)

let exec_cek (c : exp) (e : words Dynarray.t) (k : words) (m : memo_t) : words =
  let state = { c; e; k; sc = 0 } in
  let i = ref 0 in
  let rec exec state =
    log ("step " ^ string_of_int !i ^ ": " ^ string_of_int state.sc);
    i := !i + 1;
    exec (ant_step state m)
  in
  try exec state
  with DoneExec state ->
    assert (Dynarray.length state.e = 1);
    print_endline ("took " ^ string_of_int !i ^ " step, but without memo take " ^ string_of_int state.sc ^ " step.");
    Dynarray.get_last state.e
