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
let string_of_trie (t : trie) : string = match t with Leaf _ -> "Leaf" | Branch _ -> "Branch"
let choose_step x y = if x.sc > y.sc then x else y
let choose_step_option x y = match (x, y) with None, y -> y | x, None -> x | Some x, Some y -> Some (choose_step x y)

let trie_degree (x : trie) : int =
  match x with Leaf (p, _) -> (Pattern.pattern_measure p).degree | Branch br -> br.degree

let rec build x xstep y ystep (acc : Words.words) =
  let x_degree = (Pattern.pattern_measure x).degree in
  let y_degree = (Pattern.pattern_measure y).degree in
  let acc_degree = (Words.summary acc).degree in
  assert (x_degree = y_degree);
  if Pattern.pattern_is_empty x then (
    assert (Pattern.pattern_is_empty y);
    let prefix =
      if Generic.is_empty acc then Generic.empty else Pattern.pattern_cons (Pattern.PCon acc) Generic.empty
    in
    Leaf (prefix, choose_step xstep ystep))
  else (
    assert (not (Pattern.pattern_is_empty y));
    let xh, xt = Pattern.pattern_front_exn x in
    let yh, yt = Pattern.pattern_front_exn y in
    match (xh, yh) with
    | Pattern.PCon xh, Pattern.PCon yh ->
        let lcp, xh, yh = Words.lcp xh yh in
        if Generic.is_empty lcp then (
          let const = Children.create () in
          let xhh, xht = Words.words_front_exn xh in
          let xkey = Word.hash xhh in
          let xt = if Generic.is_empty xht then xt else Pattern.pattern_cons (Pattern.PCon xht) xt in
          let yhh, yht = Words.words_front_exn yh in
          let ykey = Word.hash yhh in
          let yt = if Generic.is_empty yht then yt else Pattern.pattern_cons (Pattern.PCon yht) yt in
          assert (xkey <> ykey);
          Children.set const xkey (Leaf (xt, xstep));
          Children.set const ykey (Leaf (yt, ystep));
          Branch { creator = "build disagree on cons"; degree = x_degree + acc_degree; prefix = acc; var = None; const })
        else
          let acc = Words.append acc lcp in
          let xt = if Generic.is_empty xh then xt else Pattern.pattern_cons (Pattern.PCon xh) xt in
          let yt = if Generic.is_empty yh then yt else Pattern.pattern_cons (Pattern.PCon yh) yt in
          build xt xstep yt ystep acc
    | Pattern.PCon xh, Pattern.PVar yh ->
        let yt = if yh = 1 then yt else Pattern.pattern_cons (Pattern.PVar (yh - 1)) yt in
        let var = Some (Leaf (yt, ystep)) in
        let const = Children.create () in
        let xhh, xht = Words.words_front_exn xh in
        let key = Word.hash xhh in
        let xt = if Generic.is_empty xht then xt else Pattern.pattern_cons (Pattern.PCon xht) xt in
        Children.set const key (Leaf (xt, xstep));
        Branch { creator = "build disagree on var"; degree = x_degree + acc_degree; prefix = acc; var; const }
    | Pattern.PVar _, Pattern.PCon _ -> build y ystep x xstep acc
    | Pattern.PVar xh, Pattern.PVar yh ->
        let xt = if xh = 1 then xt else Pattern.pattern_cons (Pattern.PVar (xh - 1)) xt in
        let yt = if yh = 1 then yt else Pattern.pattern_cons (Pattern.PVar (yh - 1)) yt in
        let var = Some (build xt xstep yt ystep Generic.empty) in
        let const = Children.create () in
        Branch { creator = "build disagree on var"; degree = x_degree + acc_degree; prefix = acc; var; const })

let rec insert_option (x : trie option) (prefix' : Pattern.pattern) (step' : step) : trie =
  let ret x =
    assert (trie_degree x = (Pattern.pattern_measure prefix').degree);
    x
  in
  (match x with None -> () | Some x -> assert (trie_degree x = (Pattern.pattern_measure prefix').degree));
  match x with
  | None -> ret (Leaf (prefix', step'))
  | Some (Leaf (prefix, step)) -> ret (build prefix step prefix' step' Generic.empty)
  | Some (Branch br) -> (
      let ph, pt = Pattern.pattern_front_exn prefix' in
      match ph with
      | Pattern.PCon ph ->
          let lcp, ph, br_prefix = Words.lcp ph br.prefix in
          if Generic.is_empty br_prefix then (
            if Generic.is_empty ph then
              let ph, pt = Pattern.pattern_front_exn pt in
              match ph with
              | Pattern.PCon _ -> failwith "impossible"
              | Pattern.PVar ph ->
                  let pt = if ph = 1 then pt else Pattern.pattern_cons (Pattern.PVar (ph - 1)) pt in
                  let var = Some (insert_option br.var pt step') in
                  ret (Branch { br with var })
            else
              let phh, pht = Words.words_front_exn ph in
              let key = Word.hash phh in
              let pt = if Generic.is_empty pht then pt else Pattern.pattern_cons (Pattern.PCon pht) pt in
              Children.update br.const key ~f:(fun x -> insert_option x pt step');
              ret (Branch br))
          else
            let const = Children.create () in
            let brh, brt = Words.words_front_exn br_prefix in
            let brkey = Word.hash brh in
            let x = Branch { br with prefix = brt; degree = br.degree - Words.degree br.prefix + Words.degree brt } in
            Children.set const brkey x;
            if Generic.is_empty ph then
              let ph, pt = Pattern.pattern_front_exn pt in
              match ph with
              | Pattern.PCon _ -> failwith "impossible"
              | Pattern.PVar ph ->
                  let pt = if ph = 1 then pt else Pattern.pattern_cons (Pattern.PVar (ph - 1)) pt in
                  let var = Some (Leaf (pt, step')) in
                  ret
                    (Branch
                       {
                         creator = "unexhausted lcp (var case)";
                         degree = (Pattern.pattern_measure prefix').degree;
                         prefix = lcp;
                         var;
                         const;
                       })
            else
              let phh, pht = Words.words_front_exn ph in
              let key = Word.hash phh in
              let pt = if Generic.is_empty pht then pt else Pattern.pattern_cons (Pattern.PCon pht) pt in
              Children.update const key ~f:(fun x -> insert_option x pt step');
              ret
                (Branch
                   {
                     creator = "unexhausted lcp (const case)";
                     degree = (Pattern.pattern_measure prefix').degree;
                     prefix = lcp;
                     var = None;
                     const;
                   })
      | Pattern.PVar ph ->
          let pt = if ph = 1 then pt else Pattern.pattern_cons (Pattern.PVar (ph - 1)) pt in
          if Generic.is_empty br.prefix then ret (Branch { br with var = Some (insert_option br.var pt step') })
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
let string_of_trie (t : trie) : string = match t with Stem _ -> "Stem" | Branch _ -> "Branch"
let reads_from_trie (t : trie) : reads = match t with Stem { reads; _ } -> reads | Branch { reads; _ } -> reads
let max_sc_of_trie (t : trie) : int = match t with Stem { max_sc; _ } -> max_sc | Branch { max_sc; _ } -> max_sc
let max_sc_of_trie_opt (t : trie option) : int = match t with None -> 0 | Some t -> max_sc_of_trie t

let max_sc_of_children (children : (int, trie) Hashtbl.t) : int =
  Hashtbl.fold children ~init:0 ~f:(fun ~key:_ ~data:child acc -> max acc (max_sc_of_trie child))

let max_sc_of_merging (ms : merging list) : int = List.fold ms ~init:0 ~f:(fun acc m -> max acc m.max_sc)

let make_stem ~(reads : reads) ~(step : step) ~(next : trie option) : trie =
  let max_sc = max step.sc (max_sc_of_trie_opt next) in
  Stem { reads; step; next; max_sc }

let make_branch ~(reads : reads) ~(children : (int, trie) Hashtbl.t) ~(merging : merging list) : trie =
  let max_sc = max (max_sc_of_children children) (max_sc_of_merging merging) in
  Branch { reads; children; merging; max_sc }

let set_reads_of_trie (t : trie) (r : reads) : trie =
  match t with Stem x -> Stem { x with reads = r } | Branch x -> Branch { x with reads = r }

let rec merge (x : trie) (y : trie) : trie =
  let rebase_merging (delta : reads) (m : merging) : merging =
    { reads = unmatch_reads delta m.reads; children = m.children; miss_count = 0; max_sc = m.max_sc }
  in
  match (x, y) with
  | Stem x, Stem y -> (
      let j = join_reads x.reads y.reads in
      (*print_endline
        ("Merging Stem/Stem:\n  x reads: " ^ string_of_reads x.reads ^ "\n  y reads: " ^ string_of_reads y.reads
       ^ "\n  result:  " ^ string_of_reads j.reads ^ "\n  x_rest:  " ^ string_of_reads j.x_rest ^ "\n  y_rest:  "
       ^ string_of_reads j.y_rest);*)
      match (j.x_weaken, j.y_weaken) with
      | true, true ->
          let children = Hashtbl.create (module Int) in
          let x_key, x_reads = Option.value_exn (reads_hash (Lazy.force j.reads) x.reads) in
          let y_key, y_reads = Option.value_exn (reads_hash (Lazy.force j.reads) y.reads) in
          assert (x_key <> y_key);
          Hashtbl.set children x_key (Stem { x with reads = x_reads });
          Hashtbl.set children y_key (Stem { y with reads = y_reads });
          make_branch ~reads:(Lazy.force j.reads) ~children ~merging:[]
      | false, true ->
          let y_reads = Option.value_exn (match_reads x.reads y.reads) in
          let next = merge_option x.next (Some (Stem { y with reads = y_reads })) in
          make_stem ~reads:x.reads ~step:x.step ~next
      | true, false ->
          let x_reads = Option.value_exn (match_reads y.reads x.reads) in
          let next = merge_option y.next (Some (Stem { x with reads = x_reads })) in
          make_stem ~reads:y.reads ~step:y.step ~next
      | false, false ->
          let next = merge_option x.next y.next in
          if x.step.sc >= y.step.sc then make_stem ~reads:x.reads ~step:x.step ~next
          else make_stem ~reads:y.reads ~step:y.step ~next)
  | Stem x, Branch y -> (
      let j = join_reads x.reads y.reads in
      (*print_endline
        ("Merging Stem/Branch:\n  x reads: " ^ string_of_reads x.reads ^ "\n  y reads: " ^ string_of_reads y.reads
       ^ "\n  result:  " ^ string_of_reads j.reads ^ "\n  x_rest:  " ^ string_of_reads j.x_rest ^ "\n  y_rest:  "
       ^ string_of_reads j.y_rest);*)
      match (j.x_weaken, j.y_weaken) with
      | true, true ->
          let children = Hashtbl.create (module Int) in
          let x_key, x_reads = Option.value_exn (reads_hash (Lazy.force j.reads) x.reads) in
          Hashtbl.set children x_key (Stem { x with reads = x_reads });
          (*Hashtbl.iter y.children ~f:(fun child_trie ->
              let child_key, child_reads =
                Option.value_exn (reads_hash (Lazy.force j.reads) (unmatch_reads y.reads (reads_from_trie child_trie)))
              in
              Hashtbl.update children child_key ~f:(insert_option (set_reads_of_trie child_trie child_reads)));*)
          make_branch ~reads:(Lazy.force j.reads) ~children ~merging:[]
          (*{ reads = y.reads; children = y.children; miss_count = 0; max_sc = y.max_sc }
            :: List.map y.merging ~f:(rebase_merging y.reads);*)
      | false, true ->
          let y_reads = Option.value_exn (match_reads x.reads y.reads) in
          let next = merge_option x.next (Some (Branch { y with reads = y_reads })) in
          make_stem ~reads:x.reads ~step:x.step ~next
      | true, false ->
          let x_key, x_reads = Option.value_exn (reads_hash y.reads x.reads) in
          Hashtbl.update y.children x_key ~f:(fun existing ->
              let merged = insert_option (Stem { x with reads = x_reads }) existing in
              let merged_max = max_sc_of_trie merged in
              if merged_max > y.max_sc then y.max_sc <- merged_max;
              merged);
          Branch y
      | _ ->
          failwith
            ("merge not implemented yet for Stem/Branch:" ^ string_of_bool j.x_weaken ^ "," ^ string_of_bool j.y_weaken)
      )
  | Branch x, Stem y -> merge (Stem y) (Branch x)
  | Branch x, Branch y -> (
      let j = join_reads x.reads y.reads in
      match (j.x_weaken, j.y_weaken) with
      | true, true ->
          let children = Hashtbl.create (module Int) in
          (*
          Hashtbl.iter x.children ~f:(fun child_trie ->
              let child_key, child_reads =
                Option.value_exn (reads_hash j.reads (unmatch_reads x.reads (reads_from_trie child_trie)))
              in
              Hashtbl.update children child_key ~f:(insert_option (set_reads_of_trie child_trie child_reads)));
          Hashtbl.iter y.children ~f:(fun child_trie ->
              let child_key, child_reads =
                Option.value_exn (reads_hash j.reads (unmatch_reads y.reads (reads_from_trie child_trie)))
              in
              Hashtbl.update children child_key ~f:(insert_option (set_reads_of_trie child_trie child_reads)));*)
          make_branch ~reads:(Lazy.force j.reads) ~children ~merging:[]
          (*merging =
            { reads = j.x_rest; children = x.children; miss_count = 0; max_sc = max_sc_of_children x.children }
            :: List.map x.merging ~f:(rebase_merging j.x_rest)
            @ { reads = j.y_rest; children = y.children; miss_count = 0; max_sc = max_sc_of_children y.children }
              :: List.map y.merging ~f:(rebase_merging j.y_rest);*)
      | true, false ->
          (*
          Hashtbl.iter x.children ~f:(fun child_trie ->
              let child_key, child_reads =
                Option.value_exn (reads_hash j.reads (unmatch_reads x.reads (reads_from_trie child_trie)))
              in
              Hashtbl.update y.children child_key ~f:(insert_option (set_reads_of_trie child_trie child_reads)));*)
          make_branch ~reads:y.reads ~children:y.children ~merging:[]
          (*merging =
            { reads = j.x_rest; children = x.children; miss_count = 0; max_sc = max_sc_of_children x.children }
            :: List.map x.merging ~f:(rebase_merging j.x_rest)
            @ y.merging;*)
      | false, true ->
          (*Hashtbl.iter y.children ~f:(fun child_trie ->
              let child_key, child_reads =
                Option.value_exn (reads_hash j.reads (unmatch_reads y.reads (reads_from_trie child_trie)))
              in
              Hashtbl.update x.children child_key ~f:(insert_option (set_reads_of_trie child_trie child_reads)));*)
          make_branch ~reads:x.reads ~children:x.children ~merging:[]
          (*merging =
            x.merging
            @ { reads = j.y_rest; children = y.children; miss_count = 0; max_sc = max_sc_of_children y.children }
              :: List.map y.merging ~f:(rebase_merging j.y_rest);*)
      | _ ->
          failwith
            ("merge not implemented yet for Branch/Branch:" ^ string_of_bool j.x_weaken ^ ","
           ^ string_of_bool j.y_weaken))

and merge_option (x : trie option) (y : trie option) : trie option =
  match (x, y) with None, _ -> y | _, None -> x | Some x, Some y -> Some (merge x y)

and insert_option (x : trie) (y : trie option) : trie = match y with None -> x | Some y -> merge x y

let pattern_size (p : Pattern.pattern) = Generic.size p
let patterns_size (p : Pattern.pattern cek) : int = fold_ek p 0 (fun acc p -> acc + pattern_size p)

let patterns_pvar_length (p : Pattern.pattern cek) : int =
  fold_ek p 0 (fun acc p -> acc + Pattern.pattern_pvar_length p)

let rec list_to_pattern (x : Pattern.pattern list) : Pattern.pattern =
  match x with [] -> Generic.empty | [ h ] -> h | h :: t -> Pattern.pattern_append h (list_to_pattern t)

let insert_step (m : memo) (step : step) : unit =
  if false && patterns_size step.src > max_rule_size then (
    step.insert_time <- 0;
    ())
  else
    let start_time = Time_stamp_counter.now () in
    Array.set m step.src.c.pc
      (Some
         (insert_option (make_stem ~reads:(reads_from_patterns step.src) ~step ~next:None) (Array.get m step.src.c.pc)));
    let end_time = Time_stamp_counter.now () in
    let calibrator = Lazy.force Time_stamp_counter.calibrator in
    let elapsed_time =
      Time_stamp_counter.diff end_time start_time
      |> Time_stamp_counter.Span.to_time_ns_span ~calibrator
      |> Core.Time_ns.Span.to_int63_ns |> Core.Int63.to_int_exn
    in
    step.insert_time <- elapsed_time

let acc_sc (acc : step option) : int = match acc with None -> 0 | Some step -> step.sc

let rec lookup_step_aux (value : state) (trie : trie) (acc : step option) : step option =
  match trie with
  | Stem st -> (
      if acc_sc acc >= st.max_sc then acc
      else
        match values_hash st.reads value with
        | None -> acc
        | Some (_, value) -> (
            let acc =
              match acc with
              | None -> Some st.step
              | Some step' -> if st.step.sc > step'.sc then Some st.step else Some step'
            in
            match st.next with
            | None -> acc
            | Some child -> if acc_sc acc >= max_sc_of_trie child then acc else lookup_step_aux value child acc))
  | Branch br -> (
      br.merging <-
        List.filter_map br.merging ~f:(fun m ->
            let merge m_trie =
              let m_hash, m_reads =
                reads_hash br.reads (unmatch_reads m.reads (reads_from_trie m_trie)) |> Option.value_exn
              in
              Hashtbl.update br.children m_hash ~f:(fun existing ->
                  let merged = insert_option (set_reads_of_trie m_trie m_reads) existing in
                  let merged_max = max_sc_of_trie merged in
                  if merged_max > br.max_sc then br.max_sc <- merged_max;
                  merged)
            in
            let on_miss () =
              m.miss_count <- m.miss_count + 1;
              if m.miss_count >= Hashtbl.length m.children then (
                Hashtbl.iter m.children ~f:merge;
                None)
              else Some m
            in
            match values_hash m.reads value with
            | None -> on_miss ()
            | Some (key, _) -> (
                match Hashtbl.find m.children key with
                | None -> on_miss ()
                | Some m_trie ->
                    Hashtbl.remove m.children key;
                    if max_sc_of_trie m_trie = m.max_sc then m.max_sc <- max_sc_of_children m.children;
                    merge m_trie;
                    Some m));
      if acc_sc acc >= br.max_sc then acc
      else
        match values_hash br.reads value with
        | None -> acc
        | Some (key, value) -> (
            match Hashtbl.find br.children key with
            | None -> acc
            | Some child_trie ->
                if acc_sc acc >= max_sc_of_trie child_trie then acc else lookup_step_aux value child_trie acc))

let lookup_step (value : state) (m : memo) : step option =
  let pc = value.c.pc in
  lookup_step_aux (Array.get m pc) (list_to_value (ek_to_list value))

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

let instantiate (step : step) (state : state) : step =
  let keep_last_count = 10 in
  let join p v : Pattern.pattern =
    let pvar_count = Pattern.pattern_pvar_count p in
    let rec instantiate_aux p v (count : int) =
      assert (count >= 0);
      if count = 0 then p
      else
        let ph, pt = Pattern.pattern_front_exn p in
        match ph with
        | Pattern.PCon ph ->
            let v = Option.value_exn (Value.unwords v ph) in
            Pattern.pattern_cons (PCon ph) (instantiate_aux pt v count)
        | Pattern.PVar ph ->
            let vh, vt = Value.pop_n v ph in
            Pattern.pattern_cons (PCon (Value.value_to_words vh)) (instantiate_aux pt vt (count - 1))
    in
    if pvar_count <= keep_last_count then p else instantiate_aux p v (pvar_count - keep_last_count)
  in
  let src = zipwith_ek join step.src state in
  let dst : state = Dependency.step_through step (Dependency.pattern_to_value src) in
  { step with src; dst }

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
    match t with
    | Leaf (_, st) ->
        stem_nodes := !stem_nodes + 1;
        node_stats := { depth; insert_time = st.insert_time; node_state = Stem_node } :: !node_stats;
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
    | Branch br -> (
        branch_nodes := !branch_nodes + 1;
        node_stats := { depth; insert_time = 1; node_state = Branch_node } :: !node_stats;
        hashtable_stats := { depth; size = Children.length br.const } :: !hashtable_stats;
        Children.iter br.const ~f:(fun child -> aux child (depth + 1));
        match br.var with None -> () | Some var -> aux var (depth + 1))
  in
  Array.iter m ~f:(fun opt_trie -> match opt_trie with None -> () | Some trie -> aux trie 0);
  {
    by_depth;
    node_stat = !node_stats;
    rule_stat = !rule_stat;
    hashtable_stat = !hashtable_stats;
    node_counts = { stem_nodes = !stem_nodes; branch_nodes = !branch_nodes; total_nodes = !total_nodes };
  }
