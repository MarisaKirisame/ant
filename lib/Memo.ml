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
open Common
module Hasher = Hash.MCRC32C

(*module Hasher = Hash.MCRC32*)
(*module Hasher = Hash.SL2*)
(*module Hasher = Hash.DebugHash*)
module Hashtbl = Core.Hashtbl
open Value
open State

(* todo: when the full suffix is fetched, try to extend at front. *)
type fetch_result = { fetched : words; have_prefix : bool; have_suffix : bool }
and words = seq
(* Just have Word.t. We could make Word a finger tree of Word.t but that would cost lots of conversion between two representation. *)

let source_to_string (src : source) =
  match src with E i -> "E" ^ string_of_int i | S i -> "S" ^ string_of_int i | K -> "K"

let request_to_string (req : fetch_request) =
  "at " ^ source_to_string req.src ^ "+" ^ string_of_int req.offset ^ ", " ^ string_of_int req.word_count ^ " words"

let fr_to_fh (fr : fetch_result) : fetch_hash =
  let ret = Hasher.hash (Option.get (Generic.measure ~measure ~monoid fr.fetched).full).hash in
  ret

(*
  Get a value in `rs` with given `src`.
  Depending on the `src`, the value is fetched:
    - E i: from the env of the state inside the record_state.
           (not the env of the state which contains the record_state,
           because it is in a deeper depth and not corresponding to the current record_state)
    - S i: from the store of the record_state
    - K: the kont of the state inside the record_state, same as E i
*)

let get_value_with_store (state : state) (store : store) (src : source) : value =
  match src with E i -> Dynarray.get state.e i | S i -> Dynarray.get store i | K -> state.k

let get_value (s : state) (src : source) : value =
  match src with E i -> Dynarray.get s.e i | S _ -> failwith "get_value impossible" | K -> s.k

let add_to_store (store : store) (seq : seq) (fetch_length : int ref) : unit =
  let v = { seq; fetch_length } in
  Dynarray.add_last store v

let init_fetch_length () : int ref = ref 1

let fetch_value (state : state) (store : store) (req : fetch_request) : fetch_result option =
  let v = get_value_with_store state store req.src in
  let x, y = pop_n v.seq req.offset in
  let words, rest =
    Generic.split ~monoid ~measure
      (fun m -> not (match m.full with Some f -> f.length <= req.word_count | None -> false))
      y
  in
  let length = (Option.get (Generic.measure ~monoid ~measure words).full).length in
  assert (length <= req.word_count);
  if (not (Generic.is_empty rest)) && length != req.word_count then
    (* We could try to return the shorten fragment and continue.
     * however i doubt it is reusable so we are just cluttering the hashtable*)
    None
  else
    let have_prefix = not (Generic.is_empty x) in
    let have_suffix = not (Generic.is_empty rest) in
    if have_prefix then add_to_store store x v.fetch_length;
    if have_suffix then add_to_store store rest v.fetch_length;
    if not have_suffix then v.fetch_length := !(v.fetch_length) * 2;
    Some { fetched = words; have_prefix; have_suffix = not (Generic.is_empty rest) }

let rec subst (state : state) (store : store) (x : seq) : seq =
  let lhs, rhs = Generic.split ~monoid ~measure (fun m -> Option.is_none m.full) x in
  assert (Option.is_some (Generic.measure ~monoid ~measure lhs).full);
  match Generic.front rhs ~monoid ~measure with
  | None -> lhs
  | Some (rest, Reference r) ->
      Generic.append ~monoid ~measure lhs
        (Generic.append ~monoid ~measure (subst_reference state store r) (subst state store rest))
  | _ -> failwith "unshift_seq impossible"

and subst_reference (state : state) (store : store) (r : reference) : seq =
  let v = get_value_with_store state store r.src in
  slice v.seq r.offset r.values_count

(* reference in y get resolved to values in x and store *)
let subst_state (x : state) (store : store) (y : state) : state =
  let e = Dynarray.map (fun v -> { v with seq = subst x store v.seq }) y.e in
  let k = { y.k with seq = subst x store y.k.seq } in
  { y with e; k }

(* Stepping require an unfetched fragment. register the current state.
 * Note that the reference in request does not refer to value in s, but value one level down.
 *)
let register_memo_need_unfetched (s : state) (req : fetch_request) : seq option =
  let r = Option.get s.r in
  let ev = Option.get r.r in
  let lookup =
    match !ev with
    | BlackHole ->
        (*print_endline ("fill, pc " ^ string_of_int s.c.pc);*)
        let lookup = Hashtbl.create (module Core.Int) in
        (match get_progress s with
        | Some p -> ev := Need { next = { request = req; lookup }; progress = p }
        | None -> ev := Continue { request = req; lookup });
        lookup
    | Continue _ -> failwith "impossible case: Continue"
    | Need n ->
        assert (req = n.next.request);
        n.next.lookup
    | Done _ -> failwith "impossible case: Done"
    | Halfway _ -> failwith "impossible case: Halfway"
  in
  match fetch_value r req with
  | Some (fr, seq) ->
      let bh = ref BlackHole in
      (*print_endline ("new entry when " ^ source_to_string req.src ^ " word length " ^ string_of_int req.word_count);*)
      Hashtbl.add_exn lookup ~key:(fr_to_fh fr) ~data:bh;
      r.r <- Some bh;
      Some seq
  | None -> None

let lift_c (c : exp) : exp = c

let lift_value (s : state) (src : source) (d : depth_t) : value =
  { seq = Generic.singleton (Reference { src; offset = 0; values_count = 1 }); fetch_length = init_fetch_length () }

let print_state (cek : state) msg : unit =
  print_endline (msg ^ ": pc=" ^ string_of_int cek.c.pc ^ ", d=" ^ string_of_int cek.d)

let rec print_stacktrace (s : state) : unit =
  print_state s "stacktrace";
  match s.r with Some s -> print_stacktrace s.m | _ -> ()

let rec enter_new_memo (s : state) (m : memo_t) : state =
  enter_new_memo_aux
    { m = s; s = Dynarray.create (); l = Hashtbl.create (module Source); f = 0; r = None }
    (Array.get m s.c.pc) None 0

and rs_insert_memo_node (rs : record_state) (m : memo_node_t ref) : unit =
  assert (rs.r = None);
  rs.r <- Some m

and enter_new_memo_aux (rs : record_state) (m : memo_node_t ref) (p : progress_t option) (depth : int) : state =
  let try_enter_next next progress =
    match fetch_value rs next.request with
    | Some (fr, _) -> (
        match Hashtbl.find next.lookup (fr_to_fh fr) with
        | None -> (
            let bh = ref BlackHole in
            Hashtbl.add_exn next.lookup ~key:(fr_to_fh fr) ~data:bh;
            rs_insert_memo_node rs bh;
            match progress with
            | Some p -> p.enter rs
            | None -> failwith "todo: should make a new record_state. we are not skipping ahead but that's fine")
        | Some m -> enter_new_memo_aux rs m progress (depth + 1))
    | None -> (
        rs_insert_memo_node rs m;
        match progress with Some p -> p.enter rs | None -> rs.m)
  in
  match !m with
  | Halfway p ->
      rs_insert_memo_node rs m;
      failwith "halfway" (*p.enter rs*)
  | Done d ->
      (*todo: d.skip should allow rs.r to be in whatever state as it is done.*)
      rs_insert_memo_node rs m;
      d.skip rs
  | BlackHole -> (
      match p with
      | None ->
          m := BlackHole;
          rs_insert_memo_node rs m;
          {
            c = lift_c rs.m.c;
            e = Dynarray.init (Dynarray.length rs.m.e) (fun i -> lift_value rs.m (E i) rs.m.d);
            k = lift_value rs.m K rs.m.d;
            d = rs.m.d + 1;
            sc = rs.m.sc;
            r = Some rs;
          }
      | Some p ->
          rs_insert_memo_node rs m;
          p.enter rs)
  | Need { next; progress } -> try_enter_next next (Some progress)
  | Continue next -> try_enter_next next p

(* A single transition step should:
 *   0   - Start with a sequence of resolve
 *   1.0 - If any resolve return None, call record_memo_exit
 *   1.1 - Otherwise, issue a sequence of writes to state
 *   1.2 - Or call exec_done if everything had been evaluated
 * A key invariant is that all resolve must appear before any action in state 1.
 *   In particular, this mean you should not resolve once you write to state.
 *)
exception DoneExc

let rec resolve_seq (s : state) (x : seq) : (Word.t * seq) option =
  let m = Generic.measure ~monoid ~measure x in
  assert (m.degree = 1);
  assert (m.max_degree = 1);
  let tl, hd = Generic.front_exn ~monoid ~measure x in
  match hd with
  | Direct w -> Some (w, tl)
  | Indirect (_, ref) ->
      let rs = Option.get s.r in
      let r_v = get_value_rs rs ref.src in
      if r_v.depth = s.d then
        resolve_seq s (Generic.append ~monoid ~measure (slice r_v.seq ref.offset ref.values_count) tl)
      else (
        assert (r_v.depth + 1 = s.d);
        match
          register_memo_need_unfetched s { src = ref.src; offset = ref.offset; word_count = !(r_v.fetch_length) }
        with
        | Some seq -> (
            let seq_tl, seq_hd = Generic.front_exn ~monoid ~measure (slice seq ref.offset ref.values_count) in
            let rest = Generic.append ~monoid ~measure seq_tl tl in
            match seq_hd with Direct w -> Some (w, rest) | Indirect _ -> failwith "impossible: indirect")
        | None -> None)

(* Todo: I think we should path-compress lazily all places in the code, just like what we are doing here. *)
(* Src cannot be a Store, as we are resolving location at the top level, while only non-top-level have store. *)
let rec resolve (s : state) (src : source) : (Word.t * seq) option =
  let v = get_value s src in
  assert ((Generic.measure ~monoid ~measure v.seq).degree = 1);
  assert ((Generic.measure ~monoid ~measure v.seq).max_degree = 1);
  match resolve_seq s v.seq with
  | Some ret -> (
      match s.r with
      | Some r ->
          set_value_rs r src
            {
              seq = Generic.cons ~monoid ~measure (snd ret) (Direct (fst ret));
              depth = v.depth;
              compressed_since = v.compressed_since;
              fetch_length = v.fetch_length;
            };
          Some ret
      | None -> Some ret)
  | None -> None

let rec exec_done (s : state) : 'a =
  match s.r with
  | Some r ->
      let ev = Option.get r.r in
      (match !ev with BlackHole -> ev := Done (get_done s) | _ -> failwith "exec_done impossible");
      exec_done (record_memo_exit s)
  | None -> raise DoneExc

let pc_map : exp Dynarray.t = Dynarray.create ()

let add_exp (f : state -> state) (pc_ : int) : unit =
  let pc = Dynarray.length pc_map in
  assert (pc == pc_);
  Dynarray.add_last pc_map { step = f; pc }

let pc_to_exp (pc : int) : exp = Dynarray.get pc_map pc

let value_at_depth (seq : seq) (depth : int) : value =
  let m = Generic.measure ~monoid ~measure seq in
  if m.degree <> 1 then print_endline (string_of_int m.degree);
  assert (m.degree = 1);
  assert (m.max_degree = 1);
  { seq; depth; fetch_length = init_fetch_length (); compressed_since = 0 }

let from_constructor (ctag : int) : seq = Generic.singleton (Direct (Word.make Word.constructor_tag ctag))
let from_int (i : int) : seq = Generic.singleton (Direct (Word.make Word.int_tag i))

let to_int (s : seq) : int =
  assert ((Generic.measure ~monoid ~measure s).degree = 1);
  assert ((Generic.measure ~monoid ~measure s).max_degree = 1);
  assert (Generic.size s = 1);
  match Generic.head_exn s with Direct w -> w | Indirect _ -> failwith "conveting indirect to_int"

let append (x : seq) (y : seq) : seq = Generic.append ~monoid ~measure x y
let appends (x : seq list) : seq = List.fold_right append x empty
let pop (s : seq) = pop_n s 1

let rec splits (x : seq) : seq list =
  if is_empty x then []
  else
    let h, t = pop x in
    h :: splits t

let list_match (x : seq) : (Word.t * seq) option =
  Option.map (fun (x, Direct y) -> (y, x)) (Generic.front ~monoid ~measure x)

let push_env (s : state) (v : value) : unit =
  assert ((Generic.measure ~monoid ~measure v.seq).degree = 1);
  assert ((Generic.measure ~monoid ~measure v.seq).max_degree = 1);
  Dynarray.add_last s.e v

let pop_env (s : state) : value =
  let v = Dynarray.pop_last s.e in
  assert ((Generic.measure ~monoid ~measure v.seq).degree = 1);
  assert ((Generic.measure ~monoid ~measure v.seq).max_degree = 1);
  v

let env_call (s : state) (keep : int list) (nargs : int) : seq =
  let l = Dynarray.length s.e in
  let ret = appends (List.map (fun i -> (Dynarray.get s.e i).seq) keep) in
  s.e <- Dynarray.init nargs (fun i -> Dynarray.get s.e (l - nargs + i));
  assert ((Generic.measure ~monoid ~measure ret).degree = List.length keep);
  assert ((Generic.measure ~monoid ~measure ret).max_degree = List.length keep);
  ret

let restore_env (s : state) (n : int) (seqs : seq) : unit =
  let splitted = List.rev (List.tl (List.rev (splits seqs))) in
  assert (List.length splitted = n);
  assert (Dynarray.length s.e = 1);
  let last = Dynarray.get_last s.e in
  s.e <- Dynarray.of_list (List.map (fun x -> value_at_depth x s.d) splitted);
  Dynarray.add_last s.e last

let get_next_cont (seqs : seq) : seq =
  let splitted = splits seqs in
  List.hd (List.rev splitted)

let stepped (x : state) =
  x.sc <- x.sc + 1;
  x

let return_n (s : state) (n : int) (return_exp : exp) : state =
  assert (Dynarray.length s.e = n);
  s.e <- Dynarray.of_list [ Dynarray.get_last s.e ];
  s.c <- return_exp;
  stepped s

let drop_n (s : state) (e : int) (n : int) (return_exp : exp) : state =
  assert (Dynarray.length s.e = e);
  let last = Dynarray.pop_last s.e in
  let rec loop x =
    if x = n then ()
    else (
      Dynarray.remove_last s.e;
      loop (x + 1))
  in
  loop 0;
  Dynarray.add_last s.e last;
  s.c <- return_exp;
  stepped s

let assert_env_length (s : state) (e : int) : unit =
  let l = Dynarray.length s.e in
  if l <> e then print_endline ("env_length should be " ^ string_of_int e ^ " but is " ^ string_of_int l);
  assert (l = e)

(* Contribution and CEK context:
 * Each record context is required to made contribution to the memo tree eventually,
 *   by for example, inserting a new entry, or updating a new entry from a `black hole` to `need`.
 * To enforce this, the machine behave differently depending on
 *   whether the current record context have made contribution or not.
 * In particular, only when the current record context have made contribution,
 *   can the machine exit the record context or enter another one.
 *)

type step_status = NoContribution | Contributed | NoProgress
(*
 * A step might result in 1 of 3 status:
 * - NoContribution: where the enter made a new record context which no contribution had been made yet.
 *     We then have to stay in the current record context until contribution is made.
 * - Contributed: where the machine made a contribution. This can be due to:
 *   - Creating a new entry in the memo tree, and moving down into a corresponding memo context.
 *   - Making progress in the current record context, as those progress is guranteed to be uploaded to the memo tree.
 *   - Exiting the current record context, moving up out of it (as previos record context made contribution).
 *     In this case, everything is good.
 * - NoProgress: where the machine did not make any progress
 *     In this case a raw machine step must be executed, to enforce progress.
 *
 * The function ant_step is the default function to progress the machine, 
 *   and can be called repeatedly to evaluate the program into a value. 
 * Ant step assume the input is in a contributed status (the root of the context stack also count for uniformity),
 *   and will return a state in a contributed status.
 *)

let rec ant_step (cek : state) (m : memo_t) : state =
  match memo_step_contributed cek m with
  | NoContribution, cek -> (
      match memo_step_no_contribution cek m with Contributed, cek -> cek | NoProgress, cek -> raw_step cek m)
  | Contributed, cek -> cek
  | NoProgress, cek -> raw_step cek m

and memo_step_no_contribution (cek : state) (_ : memo_t) : step_status * state =
  (* Search the memo table to go forward.
   *   But - will not create/move to a new context.
   *   Thus - if resolve_seq failed, or if there is no more values in tree,
   *     will just use the last progress
   *)
  1

and memo_step_contributed (cek : state) (_ : memo_t) : step_status * state =
  (* Search the memo table to go forward.
   *   If resolve_seq failed, will abort upward to a progress that can pass.
   *   If the tree does not contain the correct hash, will insert a entry, 
   *     enter the record_context and return a contributed status.
   *   If all reolving succeed and entered a non-needed leave state (such as halfway),
   *     enter the record_context and return a non_contributed status.
   *)
  1

and raw_step (cek : state) (_ : memo_t) : state = cek.c.step cek

let memo_step (cek : state) (m : memo_t) : state =
  let before_step = cek.sc in
  let cek = enter_new_memo cek m in
  if cek.sc > before_step then cek else raw_step cek m

let memo_over (cek : state) (m : memo_t) : state =
  let before_step = cek.sc in
  let before_depth = cek.d in
  let cek = enter_new_memo cek m in
  let cek = if cek.d > before_depth then record_memo_exit cek else cek in
  if cek.sc > before_step then cek else raw_step cek m

let exec_cek (c : exp) (e : words Dynarray.t) (k : words) (m : memo_t) : words =
  let init_value (w : words) : value = value_at_depth w 0 in
  let cek = { c; e = Dynarray.map init_value e; k = init_value k; d = 0; sc = 0; r = None } in
  let i = ref 0 in
  let rec exec cek =
    (*print_state cek "debug_state";*)
    i := !i + 1;
    if !i mod 100 = 0 then exec (memo_step cek m) else exec (memo_over cek m)
  in
  try exec (enter_new_memo cek m)
  with DoneExc ->
    assert (Dynarray.length cek.e = 1);
    print_endline ("took " ^ string_of_int !i ^ " step, but without memo take " ^ string_of_int cek.sc ^ " step.");
    (Dynarray.get_last cek.e).seq

let resolve_failed (cek : state) (m : memo_t) : state =
  let before_step = (Option.get cek.r).m.sc in
  let cek = record_memo_exit cek in
  if cek.sc > before_step then cek else raw_step cek m
