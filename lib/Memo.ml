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
  (*print_endline ("hash: " ^ string_of_int ret);*)
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
let get_value_rs (rs : record_state) (src : source) : value =
  match src with E i -> Dynarray.get rs.m.e i | S i -> Dynarray.get rs.s i | K -> rs.m.k

let get_value (s : state) (src : source) : value =
  match src with E i -> Dynarray.get s.e i | S _ -> failwith "get_value impossible" | K -> s.k

let set_value_rs (rs : record_state) (src : source) (v : value) : unit =
  match src with E i -> Dynarray.set rs.m.e i v | S i -> Dynarray.set rs.s i v | K -> rs.m.k <- v

let set_value (s : state) (src : source) (v : value) : unit =
  match src with E i -> Dynarray.set s.e i v | S _ -> failwith "set_value impossible" | K -> s.k <- v

(*
  Get a value in `rs` with given `src` and path-compress it.
  a value at depth 0 is trivially compressed.
  If the value is at depth + 1, directly do that with current record_state,
  otherwise, it's at depth, then we get the record_state of the previous depth and compress it.
*)
let rec path_compress (rs : record_state) (src : source) : value =
  let v = get_value_rs rs src in
  let new_v =
    if v.depth = 0 then v (*anything at depth 0 is trivially path-compressed*)
    else if v.depth = rs.m.d + 1 then path_compress_value rs v
    else if v.depth = rs.m.d then path_compress_value (Option.get rs.m.r) v
    else failwith "bad depth"
  in
  set_value_rs rs src new_v;
  new_v

(*given a last of depth x, value with depth x+1. path compress
  first we check if the value has been compressed;
  if not, we call path_compress_seq on the seq of the value
    and update the value with a more recent compressed_since value.
*)
and path_compress_value (rs : record_state) (v : value) : value =
  assert (v.depth = rs.m.d + 1);
  if v.compressed_since = rs.f then v else { v with seq = path_compress_seq rs v.seq; compressed_since = rs.f }

(*path compressing a seq with depth x+1
  path compress a seq means find the first element without a full measure_t
  and the element must be a reference, then compress the reference,
  then recursively path compress the rest of the seq
*)
and path_compress_seq (rs : record_state) (x : seq) : seq =
  let lhs, rhs = Generic.split ~monoid ~measure (fun m -> Option.is_none m.full) x in
  assert (Option.is_some (Generic.measure ~monoid ~measure lhs).full);
  match Generic.front rhs ~monoid ~measure with
  | None -> lhs
  | Some (rest, Reference y) ->
      Generic.append ~monoid ~measure lhs
        (Generic.append ~monoid ~measure (path_compress_reference rs y) (path_compress_seq rs rest))
  | _ -> failwith "path_compress_seq impossible"

(*path compressing reference of depth x+1
  first obtain the value denoted by the reference,
  then check the depth of the value:
    if the depth is the same as the current depth, then the value is already compressed, return the slice of the value
    otherwise, path compress it by getting the compressed value, update the reference to point to the compressed value,
    and return the content in the value represented by the reference
*)
and path_compress_reference (rs : record_state) (r : reference) : seq =
  let v = get_value_rs rs r.src in
  if v.depth = rs.m.d + 1 then (
    let v = path_compress_value rs v in
    set_value_rs rs r.src v;
    slice v.seq r.offset r.values_count)
  else Generic.Single (Reference r)

let add_to_store (rs : record_state) (seq : seq) (fetch_length : int ref) : seq =
  let v = { depth = rs.m.d; seq; compressed_since = 0; fetch_length } in
  let r =
    { src = S (Dynarray.length rs.s); offset = 0; values_count = (Generic.measure ~monoid ~measure seq).degree }
  in
  Dynarray.add_last rs.s v;
  Generic.singleton (Reference r)

let init_fetch_length () : int ref = ref 1

(*move a value from depth x to depth x+1*)
let fetch_value (rs : record_state) (req : fetch_request) : (fetch_result * seq) option =
  (*print_endline
    ("fetching " ^ string_of_int req.word_count ^ " words from " ^ source_to_string req.src ^ " offset "
   ^ string_of_int req.offset ^ " at depth " ^ string_of_int rs.m.d);*)
  (* Only value at the right depth can be fetched. 
   * If higher depth, it is already fetched so pointless to fetch again.
   * If lower depth, it is not fetched by the last level so we cannot trepass.
   *)
  assert ((get_value_rs rs req.src).depth = rs.m.d);
  let v = path_compress rs req.src in
  let x, y = pop_n v.seq req.offset in
  let words, rest =
    Generic.split ~monoid ~measure
      (fun m -> not (match m.full with None -> false | Some m -> m.length <= req.word_count))
      y
  in
  let length = (Option.get (Generic.measure ~monoid ~measure words).full).length in
  assert (length <= req.word_count);
  if (not (Generic.is_empty rest)) && length != req.word_count then
    (*we could try to return the shorten fragment and continue. however i doubt it is reusable so we are just cluttering the hashtable*)
    None
  else
    let transformed_x = if Generic.is_empty x then Generic.empty else add_to_store rs x v.fetch_length in
    let transformed_rest =
      if Generic.is_empty rest then Generic.empty (*todo: match in the reverse direction*)
      else add_to_store rs rest v.fetch_length
    in
    assert ((Generic.measure ~monoid ~measure transformed_rest).degree = (Generic.measure ~monoid ~measure rest).degree);
    rs.f <- rs.f + 1;
    let seq = Generic.append ~monoid ~measure transformed_x (Generic.append ~monoid ~measure words transformed_rest) in
    assert ((Generic.measure ~monoid ~measure seq).degree = (Generic.measure ~monoid ~measure v.seq).degree);
    set_value_rs rs req.src { depth = v.depth + 1; fetch_length = v.fetch_length; seq; compressed_since = rs.f };
    if not (Generic.is_empty rest) then v.fetch_length := !(v.fetch_length) * 2 else ();
    Some ({ fetched = words; have_prefix = not (Generic.is_empty x); have_suffix = not (Generic.is_empty rest) }, seq)

(*assuming this seq is at depth l.m.d+1, convert it to depth l.m.d*)
let rec unshift_seq (rs : record_state) (x : seq) : seq =
  let lhs, rhs = Generic.split ~monoid ~measure (fun m -> Option.is_none m.full) x in
  assert (Option.is_some (Generic.measure ~monoid ~measure lhs).full);
  match Generic.front rhs ~monoid ~measure with
  | None -> lhs
  | Some (rest, Reference y) ->
      Generic.append ~monoid ~measure lhs
        (Generic.append ~monoid ~measure (unshift_reference rs y) (unshift_seq rs rest))
  | _ -> failwith "unshift_seq impossible"

and unshift_reference (rs : record_state) (r : reference) : seq =
  let v = unshift_source rs r.src in
  assert (v.depth = rs.m.d);
  slice v.seq r.offset r.values_count

(*move a value from depth x to depth x-1. if it refer to other value at the current level, unshift them as well.*)
and unshift_value (rs : record_state) (v : value) : value =
  if v.depth > rs.m.d then (
    assert (v.depth = rs.m.d + 1);
    { seq = unshift_seq rs v.seq; depth = rs.m.d; fetch_length = init_fetch_length (); compressed_since = 0 })
  else v

and unshift_source (rs : record_state) (src : source) : value =
  let v = get_value_rs rs src in
  if v.depth > rs.m.d then (
    let new_v = unshift_value rs v in
    set_value_rs rs src new_v;
    new_v)
  else v

let unshift_c (s : state) : exp = s.c

let unshift_all (s : state) : state =
  let r = Option.get s.r in
  let c = unshift_c s in
  let e = Dynarray.map (fun v -> unshift_value r v) s.e in
  let k = unshift_value r s.k in
  r.m.c <- c;
  r.m.e <- e;
  r.m.k <- k;
  assert (r.m.sc <= s.sc);
  r.m.sc <- s.sc;
  r.m

let strip_c (c : exp) : exp = c

let rec get_progress (s : state) : progress_t =
  {
    enter = get_enter s;
    (* We might want hint of new values to speedup the exit process, so have it general for now. 
     * Also good to make explicit the symmetry.
     *)
    exit = record_memo_exit;
  }

(* Carefully written to make sure that unneeded values can be freed asap. *)
and get_enter (s : state) : record_state -> state =
  let c = strip_c s.c in
  let e = Dynarray.map (fun v -> v.seq) s.e in
  let k = s.k.seq in
  let r = Option.get s.r in
  assert (s.sc >= r.m.sc);
  let scd = s.sc - r.m.sc in
  assert (scd >= 0);
  fun rs ->
    let depth = rs.m.d + 1 in
    let seq_to_value s = { seq = s; depth; fetch_length = init_fetch_length (); compressed_since = 0 } in
    { c; e = Dynarray.map seq_to_value e; k = seq_to_value k; d = depth; sc = rs.m.sc + scd; r = Some rs }

and record_memo_exit (s : state) : state =
  let r = Option.get s.r in
  (match r.r with
  | Evaluating ev -> ( match !ev with BlackHole -> ev := Unknown | _ -> ())
  | Reentrance _ | Building -> ());
  unshift_all s

and get_done (s : state) : done_t =
  let p = get_progress s in
  { skip = (fun rs -> p.exit (p.enter rs)) }

let rebase_value (rs : record_state) (v : value) : value = todo "rebase_value"

let rebase (rs : record_state) : record_state =
  assert (rs.r = Building);
  let rsmr = Option.get rs.m.r in
  (match rsmr.r with
  | Evaluating ev -> (
      match !ev with
      | BlackHole | Unknown | Halfway _ ->
          ev := Halfway (get_progress rs.m);
          ev := Unknown)
  | Reentrance _ -> failwith "reentrance"
  | Building _ -> failwith "building");
  let rsm = unshift_all rs.m in
  { m = rsm; f = rs.f; s = Dynarray.map (rebase_value rs) rs.s; r = Building }

(* Stepping require an unfetched fragment. register the current state.
 * Note that the reference in request does not refer to value in s, but value one level down.
 *)
let register_memo_need_unfetched (s : state) (req : fetch_request) : seq option =
  let r = Option.get s.r in
  let lookup =
    match r.r with
    | Evaluating ev -> (
        match !ev with
        | BlackHole | Unknown ->
            (*print_endline ("fill, pc " ^ string_of_int s.c.pc);*)
            let lookup = Hashtbl.create (module Core.Int) in
            ev := Need { request = req; lookup; progress = get_progress s };
            lookup
        | Need _ -> failwith "impossible case: Need"
        | Done _ -> failwith "impossible case: Done"
        | Halfway _ -> failwith "impossible case: Done")
    | Reentrance re -> (
        match re with
        | Need n ->
            (*if req <> n.request then print_endline (request_to_string req ^ " " ^ request_to_string n.request) else ();*)
            assert (req = n.request);
            n.lookup
        | BlackHole | Unknown | Done _ | Halfway _ -> failwith "register_memo_need_unfetched impossible")
    | Building -> failwith "register_memo_need_unfetched impossible"
  in
  match fetch_value r req with
  | Some (fr, seq) ->
      let bh = ref BlackHole in
      (*print_endline ("new entry when " ^ source_to_string req.src ^ " word length " ^ string_of_int req.word_count);*)
      Hashtbl.add_exn lookup ~key:(fr_to_fh fr) ~data:bh;
      r.r <- Evaluating bh;
      Some seq
  | None -> None

let lift_c (c : exp) : exp = c

let lift_value (src : source) (d : depth_t) : value =
  {
    seq = Generic.singleton (Reference { src; offset = 0; values_count = 1 });
    depth = d + 1;
    fetch_length = init_fetch_length ();
    compressed_since = 0;
  }

let print_state (cek : state) msg : unit =
  print_endline (msg ^ ": pc=" ^ string_of_int cek.c.pc ^ ", d=" ^ string_of_int cek.d)

let rec print_stacktrace (s : state) : unit =
  print_state s "stacktrace";
  match s.r with Some s -> print_stacktrace s.m | _ -> ()

let rec enter_new_memo (s : state) (m : memo_t) : state =
  enter_new_memo_aux { m = s; s = Dynarray.create (); f = 0; r = Building } (Array.get m s.c.pc) true None 0

(*only enter if there is an existing entries. this is cheaper then enter_new_memo.*)
and try_match_memo (s : state) (m : memo_t) : state =
  enter_new_memo_aux { m = s; s = Dynarray.create (); f = 0; r = Building } (Array.get m s.c.pc) false None 0

and enter_new_memo_aux (rs : record_state) (m : memo_node_t ref) (matched : bool) (p : progress_t option) (depth : int)
    : state =
  match !m with
  | Halfway p ->
      assert (rs.r = Building);
      rs.r <- Evaluating m;
      failwith "halfway";
      p.enter rs
  (*| BlackHole ->
      print_stacktrace rs.m;
      failwith "Blackhole detected"*)
  | Done d ->
      (*todo: d.skip should allow rs.r to be in whatever state as it is done.*)
      assert (rs.r = Building);
      rs.r <- Reentrance !m;
      d.skip rs
  | Unknown | BlackHole ->
      if matched then (
        match p with
        | None ->
            (*print_endline ("no entry! pc: " ^ string_of_int rs.m.c.pc);*)
            (*print_endline "new"; Werid - this is the most frequent state by an extreme amount*)
            m := BlackHole;
            assert (rs.r = Building);
            rs.r <- Evaluating m;
            {
              c = lift_c rs.m.c;
              e = Dynarray.init (Dynarray.length rs.m.e) (fun i -> lift_value (E i) rs.m.d);
              k = lift_value K rs.m.d;
              d = rs.m.d + 1;
              sc = rs.m.sc;
              r = Some rs;
            }
        | Some p ->
            assert (rs.r = Building);
            rs.r <- Evaluating m;
            p.enter rs)
      else rs.m
  | Need n -> (
      match fetch_value rs n.request with
      | Some (fr, _) -> (
          match Hashtbl.find n.lookup (fr_to_fh fr) with
          | None ->
              (*print_endline ("new entry " ^ request_to_string n.request ^ " at memo depth " ^ string_of_int depth);*)
              let bh = ref BlackHole in
              Hashtbl.add_exn n.lookup ~key:(fr_to_fh fr) ~data:bh;
              assert (rs.r = Building);
              rs.r <- Evaluating bh;
              n.progress.enter rs
          | Some m -> enter_new_memo_aux rs m true (Some n.progress) (depth + 1))
      | None ->
          (*enter_new_memo_aux (rebase rs) m matched p depth*)
          if matched then (
            assert (rs.r = Building);
            rs.r <- Reentrance !m;
            (*print_endline
              ("request " ^ request_to_string n.request ^ " failed, entering memo depth " ^ string_of_int depth);*)
            n.progress.enter rs)
          else (
            print_endline "sad";
            rs.m))

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
  | Word w -> Some (w, tl)
  | Reference ref ->
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
            match seq_hd with Word w -> Some (w, rest) | Reference _ -> failwith "impossible: reference")
        | None -> None)

(* Todo: I think we should path-compress lazily all places in the code, just like what we are doing here. *)
(* Src cannot be a Store, as we are resolving location at the top level, while only non-top-level have store. *)
let rec resolve (s : state) (src : source) : (Word.t * seq) option =
  let v = get_value s src in
  assert ((Generic.measure ~monoid ~measure v.seq).degree = 1);
  assert ((Generic.measure ~monoid ~measure v.seq).max_degree = 1);
  match resolve_seq s v.seq with
  | Some ret ->
      set_value s src
        {
          seq = Generic.cons ~monoid ~measure (snd ret) (Word (fst ret));
          depth = v.depth;
          compressed_since = v.compressed_since;
          fetch_length = v.fetch_length;
        };
      Some ret
  | None -> None

let rec exec_done (s : state) : 'a =
  match s.r with
  | Some r ->
      (match r.r with
      | Evaluating ev -> ( match !ev with BlackHole -> ev := Done (get_done s) | _ -> failwith "exec_done impossible")
      | Reentrance _ -> ()
      | _ -> failwith "exec_done impossible");
      exec_done (record_memo_exit s)
  | None -> raise DoneExc

let pc_map : exp Dynarray.t = Dynarray.create ()

let add_exp (f : state -> state) : int =
  let pc = Dynarray.length pc_map in
  Dynarray.add_last pc_map { step = f; pc };
  pc

let pc_to_exp (pc : int) : exp = Dynarray.get pc_map pc

let value_at_depth (seq : seq) (depth : int) : value =
  let m = Generic.measure ~monoid ~measure seq in
  if m.degree <> 1 then print_endline (string_of_int m.degree);
  assert (m.degree = 1);
  assert (m.max_degree = 1);
  { seq; depth; fetch_length = init_fetch_length (); compressed_since = 0 }

let from_constructor (ctag : int) : seq = Generic.singleton (Word (Word.make Word.constructor_tag ctag))
let from_int (i : int) : seq = Generic.singleton (Word (Word.make Word.int_tag i))

let to_int (s : seq) : int =
  assert ((Generic.measure ~monoid ~measure s).degree = 1);
  assert ((Generic.measure ~monoid ~measure s).max_degree = 1);
  assert (Generic.size s = 1);
  match Generic.head_exn s with Word w -> w | Reference _ -> failwith "conveting reference to_int"

let append (x : seq) (y : seq) : seq = Generic.append ~monoid ~measure x y
let appends (x : seq list) : seq = List.fold_right append x empty
let pop (s : seq) = pop_n s 1

let rec splits (x : seq) : seq list =
  if is_empty x then []
  else
    let h, t = pop x in
    h :: splits t

let list_match (x : seq) : (Word.t * seq) option =
  Option.map (fun (x, Word y) -> (y, x)) (Generic.front ~monoid ~measure x)

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

let raw_step (cek : state) (_ : memo_t) : state = cek.c.step cek

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

let lookup_step (cek : state) (m : memo_t) : state = raw_step (try_match_memo cek m) m

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
