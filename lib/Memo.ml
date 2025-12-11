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

let init_memo () : memo = ref []

let lookup_step (value : state) (m : memo) : step option =
  let rec aux acc = function
    | [] -> acc
    | step :: rest ->
        let acc =
          if Dependency.can_step_through step value then
            match acc with None -> Some step | Some step' -> if step.sc > step'.sc then Some step else Some step'
          else acc
        in
        aux acc rest
  in
  aux None !m

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
  let raw_step s =
    let w = make_world (copy_state s) m in
    s.c.step w;
    w.state
  in
  let rec raw_step_n s n = if n = 0 then s else raw_step_n (raw_step s) (n - 1) in
  let dbg_step_through step state =
    assert (step.sc > 0);
    let x = Dependency.step_through step state in
    let y = raw_step_n state step.sc in
    if not (Dependency.state_equal x y) then (
      print_endline "state before step:";
      print_endline (string_of_cek state);
      print_endline "state after step:";
      print_endline (string_of_cek x);
      print_endline "expected:";
      print_endline (string_of_cek y));
    assert (Dependency.state_equal x y);
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
    m := step :: !m;
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
          m := step :: !m;
          hist := inc compose_slice { state = old; step } !hist;
          let st = dbg_step_through step old in
          exec st)
  in
  let state = exec state in
  assert (Dynarray.length state.e = 1);
  ignore (fold_bin compose_slice None !hist);
  print_endline ("took " ^ string_of_int !i ^ " step, but without memo take " ^ string_of_int !sc ^ " step.");
  { words = Dynarray.get_last state.e; step = !i; without_memo_step = !sc }

let exec_done _ = failwith "exec is done, should not call step anymore"
