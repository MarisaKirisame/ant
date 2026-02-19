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
  match x with
  | Reference r -> subst_reference resolve r
  | Node (label, children) -> Node (label, List.map children ~f:(subst resolve))

and subst_reference (resolve : source -> seq option) (r : reference) : seq =
  match resolve r.src with Some seq -> seq | None -> Reference r

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
  match x with
  | Reference r -> r :: rs
  | Node (_, children) -> List.fold_left children ~init:rs ~f:(fun acc c -> val_refs_aux c acc)

let state_refs (state : state) : reference list =
  let e = Dynarray.fold_left (fun rs x -> val_refs_aux x rs) [] state.e in
  let k = val_refs_aux state.k e in
  k

let rec resolve (w : world) (src : source) : Word.t * seq =
  set_value w.resolved src true;
  let v = get_value w.state src in
  match v with
  | Reference _ -> failwith "cannot resolve reference"
  | Node (label, children) -> (label, Value.pack children)

let pc_map : exp Dynarray.t = Dynarray.create ()
let reset () = Dynarray.clear pc_map

let add_exp (f : world -> unit) (pc_ : int) : unit =
  let pc = Dynarray.length pc_map in
  assert (Int.equal pc pc_);
  Dynarray.add_last pc_map { step = f; pc }

let pc_to_exp (Pc pc) : exp = Dynarray.get pc_map pc
let from_constructor (ctag : int) : seq = Node (Word.ConstructorTag ctag, [])
let from_int (i : int) : seq = Node (Word.Int i, [])

let to_word (s : seq) : Word.t =
  match s with
  | Node (label, []) -> label
  | Reference _ -> failwith "converting reference to word"
  | Node (_, _ :: _) -> failwith "converting non-leaf node to word"

let append (x : seq) (y : seq) : seq = Value.pack (Value.unpack_seq x @ Value.unpack_seq y)
let empty_seq : seq = Value.pack []
let flatten_seqs (xs : seq list) : seq list = List.concat_map xs ~f:Value.unpack_seq

let appends (x : seq list) : seq =
  match x with
  | [] -> empty_seq
  | Node (ConstructorTag tag, []) :: rest when tag <> Value.pack_tag -> Node (ConstructorTag tag, flatten_seqs rest)
  | _ -> Value.pack (flatten_seqs x)

let splits (x : seq) : seq list = Value.unpack_seq x
let rec splits_1 x = match splits x with h :: _ -> h | [] -> failwith "splits_1: empty"
let rec splits_2 x = match splits x with h1 :: h2 :: _ -> (h1, h2) | _ -> failwith "splits_2: not enough elements"

let rec splits_3 x =
  match splits x with h1 :: h2 :: h3 :: _ -> (h1, h2, h3) | _ -> failwith "splits_3: not enough elements"

let rec splits_4 x =
  match splits x with h1 :: h2 :: h3 :: h4 :: _ -> (h1, h2, h3, h4) | _ -> failwith "splits_4: not enough elements"

let list_match (x : seq) : (Word.t * seq) option =
  match x with
  | Reference _ -> failwith "list_match on Reference"
  | Node (label, children) -> Some (label, Value.pack children)

let push_env (w : world) (v : value) : unit = Dynarray.add_last w.state.e v

let pop_env (w : world) : value =
  let v = Dynarray.pop_last w.state.e in
  v

let env_call (w : world) (keep : int list) (nargs : int) : seq =
  let l = Dynarray.length w.state.e in
  let keep_values = List.map keep ~f:(fun i -> Dynarray.get w.state.e i) in
  let ret = Value.pack keep_values in
  w.state.e <- Dynarray.init nargs (fun i -> Dynarray.get w.state.e (l - nargs + i));
  ret

let restore_env (w : world) (n : int) (seqs : seq) : unit =
  let parts = splits seqs in
  let keep_parts = match List.rev parts with [] -> [] | _last :: rev_keep -> List.rev rev_keep in
  let keep_values =
    if List.length keep_parts = n then keep_parts
    else if n <= 0 then []
    else if n = 1 then [ Value.pack keep_parts ]
    else if List.length keep_parts > n then
      let prefix = List.take keep_parts (n - 1) in
      let rest = List.drop keep_parts (n - 1) in
      prefix @ [ Value.pack rest ]
    else keep_parts @ List.init (n - List.length keep_parts) (fun _ -> empty_seq)
  in
  let last = Dynarray.get_last w.state.e in
  w.state.e <- Dynarray.of_list keep_values;
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
let choose_step x y = if x.sc > y.sc then x else y
let choose_step_option x y = match (x, y) with None, y -> y | x, None -> x | Some x, Some y -> Some (choose_step x y)

type token = TVar | TNode of Word.t * int

let token_key (label : Word.t) (arity : int) : int = Stdlib.Hashtbl.hash (Word.hash label, arity)

let rec tokens_of_pattern (p : Pattern.pattern) : token list =
  match p with
  | Pattern.PVar _ -> [ TVar ]
  | Pattern.PNode (label, children) ->
      TNode (label, List.length children) :: List.concat_map children ~f:tokens_of_pattern

let rec tokens_of_value (v : Value.value) : token list =
  match v with
  | Reference _ -> failwith "tokens_of_value: reference in value"
  | Node (label, children) -> TNode (label, List.length children) :: List.concat_map children ~f:tokens_of_value

let compute_subtree_end (tokens : token array) : int array =
  let n = Array.length tokens in
  let end_pos = Array.create ~len:n 0 in
  let rec compute i =
    match tokens.(i) with
    | TVar ->
        end_pos.(i) <- i + 1;
        i + 1
    | TNode (_, arity) ->
        let j = ref (i + 1) in
        for _ = 1 to arity do
          j := compute !j
        done;
        end_pos.(i) <- !j;
        !j
  in
  if n = 0 then end_pos
  else (
    ignore (compute 0);
    end_pos)

let empty_trie () : trie = { steps = None; var = None; const = Children.create (); max_sc = 0 }
let update_max_sc (t : trie) (step : step) : unit = if step.sc > t.max_sc then t.max_sc <- step.sc

let rec insert_tokens (t : trie) (tokens : token list) (step : step) : unit =
  update_max_sc t step;
  match tokens with
  | [] -> t.steps <- Some (match t.steps with None -> step | Some s -> choose_step s step)
  | TVar :: rest ->
      let child =
        match t.var with
        | Some child -> child
        | None ->
            let child = empty_trie () in
            t.var <- Some child;
            child
      in
      insert_tokens child rest step
  | TNode (label, arity) :: rest ->
      let key = token_key label arity in
      let child =
        match Children.find t.const key with
        | Some child -> child
        | None ->
            let child = empty_trie () in
            Children.set t.const key child;
            child
      in
      insert_tokens child rest step

let pattern_size (p : Pattern.pattern) = Pattern.pattern_size p
let patterns_size (p : Pattern.pattern cek) : int = fold_ek p 0 (fun acc p -> acc + pattern_size p)

let patterns_pvar_length (p : Pattern.pattern cek) : int =
  fold_ek p 0 (fun acc p -> acc + Pattern.pattern_pvar_length p)

let pack_pattern (ps : Pattern.pattern list) : Pattern.pattern = Pattern.pack ps
let pack_value (vs : Value.value list) : Value.value = Value.pack vs

let insert_step (m : memo) (step : step) : unit =
  let start_time = Timer.create () in
  let end_time = Timer.create () in
  Timer.record start_time;
  let packed = pack_pattern (ek_to_list step.src) in
  let tokens = tokens_of_pattern packed in
  let root =
    match Array.get m step.src.c.pc with
    | Some t -> t
    | None ->
        let t = empty_trie () in
        Array.set m step.src.c.pc (Some t);
        t
  in
  insert_tokens root tokens step;
  Timer.record end_time;
  let elapsed_time = Timer.diff_nanoseconds start_time end_time |> Int64.to_int_exn in
  step.insert_time <- elapsed_time

let rec lookup_tokens (t : trie) (tokens : token array) (end_pos : int array) (pos : int) (expected_end : int) :
    step option =
  if pos = expected_end then t.steps
  else if pos > expected_end then None
  else
    let best =
      match t.var with None -> None | Some var -> lookup_tokens var tokens end_pos end_pos.(pos) expected_end
    in
    match tokens.(pos) with
    | TVar -> best
    | TNode (label, arity) ->
        let key = token_key label arity in
        let const =
          match Children.find t.const key with
          | None -> None
          | Some child -> lookup_tokens child tokens end_pos (pos + 1) expected_end
        in
        choose_step_option best const

let lookup_step (value : state) (m : memo) : step option =
  let pc = value.c.pc in
  match Array.get m pc with
  | None -> None
  | Some t -> (
      let packed = pack_value (ek_to_list value) in
      let tokens = tokens_of_value packed |> Array.of_list in
      if Array.length tokens = 0 then None
      else
        let end_pos = compute_subtree_end tokens in
        let expected_end = end_pos.(0) in
        match lookup_tokens t tokens end_pos 0 expected_end with
        | Some step as res -> if Dependency.can_step_through step value then res else None
        | None -> None)

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

let exec_cek_slot = Profile.register_slot Profile.memo_profile "exec_cek"
let step_through_slot = Profile.register_slot Profile.memo_profile "step_through"
let compose_step_slot = Profile.register_slot Profile.memo_profile "compose_step"
let insert_step_slot = Profile.register_slot Profile.memo_profile "insert_step"
let lookup_step_slot = Profile.register_slot Profile.memo_profile "lookup_step"
let instantiate (step : step) (_state : state) : step = step

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
      let step = instantiate step x.state in
      Profile.with_slot insert_step_slot (fun () -> insert_step m step);
      (*let lookuped = lookup_step x.state m |> Option.value_exn in
      if lookuped != step then
        failwith
          ("composed step is different from looked up step:" ^ Dependency.string_of_step step ^ "lookuped: "
         ^ Dependency.string_of_step lookuped);*)
      { state = x.state; step }
    in
    let rec exec state =
      if is_done state then state
      else (
        (*let _ = map_ek (fun v -> assert (value_valid v)) state in*)
        log ("step " ^ string_of_int !i ^ ": " ^ string_of_int !sc);
        i := !i + 1;
        match Profile.with_slot lookup_step_slot (fun () -> lookup_step state m) with
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

type node_state = Stem_node | Branch_node
type node_counts = { stem_nodes : int; branch_nodes : int; total_nodes : int }
type hashtable_stat = { depth : int; size : int }

type memo_stats = {
  by_depth : by_depth Dynarray.t;
  node_stat : node_stat list;
  rule_stat : rule_stat list;
  hashtable_stat : hashtable_stat list;
  node_counts : node_counts;
}

and by_depth = { depth : int; mutable node_count : int }
and node_stat = { depth : int; insert_time : int; node_state : node_state }

and rule_stat = {
  size : int;
  pvar_length : int;
  sc : int;
  hit_count : int;
  insert_time : int;
  depth : int;
  rule : string Lazy.t;
}

let memo_stats (m : memo) : memo_stats =
  let by_depth = Dynarray.create () in
  let node_stats = ref [] in
  let rule_stat = ref [] in
  let hashtable_stats = ref [] in
  let stem_nodes = ref 0 in
  let branch_nodes = ref 0 in
  let total_nodes = ref 0 in
  let rec aux (t : trie) (depth : int) : unit =
    if Dynarray.length by_depth <= depth then Dynarray.add_last by_depth { depth; node_count = 0 };
    let by_depth_stat = Dynarray.get by_depth depth in
    by_depth_stat.node_count <- by_depth_stat.node_count + 1;
    total_nodes := !total_nodes + 1;
    let has_children = Option.is_some t.var || Children.length t.const > 0 in
    if has_children then (
      branch_nodes := !branch_nodes + 1;
      node_stats := { depth; insert_time = 1; node_state = Branch_node } :: !node_stats;
      hashtable_stats := { depth; size = Children.length t.const } :: !hashtable_stats;
      Children.iter t.const ~f:(fun child -> aux child (depth + 1));
      match t.var with None -> () | Some var -> aux var (depth + 1))
    else (
      stem_nodes := !stem_nodes + 1;
      node_stats := { depth; insert_time = 1; node_state = Stem_node } :: !node_stats);
    match t.steps with
    | None -> ()
    | Some st ->
        rule_stat :=
          {
            size = patterns_size st.src;
            pvar_length = patterns_pvar_length st.src;
            sc = st.sc;
            hit_count = st.hit;
            insert_time = st.insert_time;
            depth;
            rule = lazy (Dependency.string_of_step st);
          }
          :: !rule_stat
  in
  Array.iter m ~f:(fun opt_trie -> match opt_trie with None -> () | Some trie -> aux trie 0);
  {
    by_depth;
    node_stat = !node_stats;
    rule_stat = !rule_stat;
    hashtable_stat = !hashtable_stats;
    node_counts = { stem_nodes = !stem_nodes; branch_nodes = !branch_nodes; total_nodes = !total_nodes };
  }
