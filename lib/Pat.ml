open Syntax

let compile ast = ast

type occurrence = int list [@@deriving show]

let rec pp_occ = function
  | [] -> PPrint.string "\\"
  | x :: xs -> PPrint.(pp_occ xs ^^ string "." ^^ string (string_of_int x))

module OccurrenceMap = Map.Make (struct
  type t = occurrence

  let compare = compare
end)

module StringMap = Map.Make (struct
  type t = string

  let compare = String.compare
end)

type pattern_matrix = {
  arity : int;
  occs : occurrence list;
  bnds : string OccurrenceMap.t list;
  pats : pattern list list;
  acts : int list;
}

let[@tail_mod_cons] rec map3 f l1 l2 l3 =
  match (l1, l2, l3) with
  | [], [], [] -> []
  | [ a1 ], [ b1 ], [ c1 ] ->
      let r1 = f a1 b1 c1 in
      [ r1 ]
  | a1 :: a2 :: l1, b1 :: b2 :: l2, c1 :: c2 :: l3 ->
      let r1 = f a1 b1 c1 in
      let r2 = f a2 b2 c2 in
      r1 :: r2 :: map3 f l1 l2 l3
  | _, _, _ -> invalid_arg "map3"

let rec unzip_map3 f l1 l2 l3 =
  match (l1, l2, l3) with
  | [], [], [] -> ([], [], [])
  | [ a1 ], [ b1 ], [ c1 ] ->
      let ra1, rb1, rc1 = f a1 b1 c1 in
      ([ ra1 ], [ rb1 ], [ rc1 ])
  | a1 :: a2 :: l1, b1 :: b2 :: l2, c1 :: c2 :: l3 ->
      let ra1, rb1, rc1 = f a1 b1 c1 in
      let ra2, rb2, rc2 = f a2 b2 c2 in
      let rl1, rl2, rl3 = unzip_map3 f l1 l2 l3 in
      (ra1 :: ra2 :: rl1, rb1 :: rb2 :: rl2, rc1 :: rc2 :: rl3)
  | _, _, _ -> invalid_arg "unzip_map3"

(*
  output format:

  arity = 4
  occurrence = [\.0, \.1, \.2, \.3]
  
  (PAny, PAny, PAny, PAny)   -> 1 with { \.0 -> x, \.1 -> y, \.2 -> z, \.3 -> w }
  (PInt 1, PAny, PAny, PAny) -> 2 with { }
  (PInt 2, PAny, PAny, PAny) -> 3 with { \.0 -> z }
  (PInt 3, PAny, PAny, PAny) -> 4 
  (PInt 4, PAny, PAny, PAny) -> 5
*)

let pp_pattern_matrix { arity; occs; bnds; pats; acts } =
  PPrint.(
    group @@ string "arity = "
    ^^ (string @@ string_of_int arity)
    ^^ hardline ^^ string "occurrence = "
    ^^ pp_occ (List.hd occs)
    ^^ hardline ^^ separate hardline
    @@ map3
         (fun pat bnd act ->
           parens (separate_map (string ",") pp_pattern pat)
           ^^ space ^^ string "->" ^^ space
           ^^ string (string_of_int act)
           ^^
           if OccurrenceMap.is_empty bnd then empty
           else
             string "with" ^^ space
             ^^ separate2 (string "(") (string ")")
                  ( OccurrenceMap.to_list bnd |> fun bnd ->
                    List.map (fun (k, v) -> pp_occ k ^^ string "->" ^^ string v) bnd ))
         pats bnds acts)

type patdesc = PDInt of int | PDBool of bool | PDUnit | PDCtor of string * int | PDTuple of int [@@deriving show]

let pat_desc pattern =
  match pattern with
  | PInt n -> PDInt n
  | PBool b -> PDBool b
  | PUnit -> PDUnit
  | PApp (ctor, None) -> PDCtor (ctor, 0)
  | PApp (ctor, Some (PTup args)) -> PDCtor (ctor, List.length args)
  | PApp (ctor, Some _) -> PDCtor (ctor, 1)
  | PTup args -> PDTuple (List.length args)
  | _ -> failwith "Invalid pattern"

let is_compatible desc pat =
  match (desc, pat) with
  | PDInt x, PInt y -> x = y
  | PDBool x, PBool y -> x = y
  | PDUnit, PUnit -> true
  | PDCtor (ctor, n), PApp (ctor', Some (PTup args)) when ctor = ctor' -> List.length args = n
  | PDCtor (ctor, n), PApp (ctor', None) when ctor = ctor' -> n = 0
  | PDCtor (ctor, n), PApp (ctor', Some _) when ctor = ctor' -> n = 1
  | PDTuple n, PTup args -> List.length args = n
  | _ -> false

let spec_cell ctor pat n occ =
  match pat with
  | PAny ->
      let new_row = List.init n (fun _ -> PAny) in
      (new_row, Some OccurrenceMap.empty)
  | (PInt _ | PBool _ | PUnit) when is_compatible ctor pat ->
      assert (n = 0);
      ([], Some OccurrenceMap.empty)
  | PVar x ->
      let new_row = List.init n (fun _ -> PAny) in
      let new_bnd = OccurrenceMap.singleton occ x in
      (new_row, Some new_bnd)
  | PTup xs when is_compatible ctor pat ->
      let new_row = xs in
      (new_row, Some OccurrenceMap.empty)
  | PApp (_, Some (PTup args)) when is_compatible ctor pat ->
      let new_row = args in
      (new_row, Some OccurrenceMap.empty)
  | PApp (_, Some arg) when is_compatible ctor pat ->
      let new_row = [ arg ] in
      (new_row, Some OccurrenceMap.empty)
  | PApp (_, None) when is_compatible ctor pat -> ([], Some OccurrenceMap.empty)
  | _ -> ([], None)

let spec_row ctor n col occs row bnd =
  let cell_n = List.nth row col in
  let occ_n = List.nth occs col in
  let cells, kept = spec_cell ctor cell_n n occ_n in
  Option.map
    (fun new_bnd ->
      ( List.flatten @@ List.mapi (fun i cell -> if i = col then cells else [ cell ]) row,
        OccurrenceMap.union (fun _k _v1 v2 -> Some v2) bnd new_bnd ))
    kept

let arity_of arity_map desc =
  match desc with
  | PDInt _ -> 0
  | PDBool _ -> 0
  | PDUnit -> 0
  | PDCtor (s, _) -> (
      match StringMap.find_opt s arity_map with
      | Some n -> n
      | None -> failwith ("Constructor " ^ s ^ " not found in arity map"))
  | PDTuple n -> n

let check_arity arity_map pat =
  let compare_old_and_new s nu =
    match StringMap.find_opt s arity_map with
    | Some old when old = nu -> arity_map
    | Some old -> failwith ("Arity mismatch for " ^ s ^ ": " ^ string_of_int old ^ " vs " ^ string_of_int nu)
    | None -> StringMap.add s nu arity_map
  in
  match pat with
  | PApp (ctor, None) -> compare_old_and_new ctor 0
  | PApp (ctor, Some (PTup args)) -> compare_old_and_new ctor (List.length args)
  | PApp (ctor, Some _) -> compare_old_and_new ctor 1
  | _ -> arity_map

let split_at n =
  let rec aux acc n l =
    match (n, l) with 0, _ -> (List.rev acc, l) | _, [] -> ([], []) | n, x :: xs -> aux (x :: acc) (n - 1) xs
  in
  aux [] n

let remove_at n =
  let rec aux acc n l =
    match (n, l) with
    | 0, _ :: xs -> List.rev_append acc xs
    | _, [] -> List.rev acc
    | n, x :: xs -> aux (x :: acc) (n - 1) xs
  in
  aux [] n

let unpack_nth occs n m =
  List.mapi (fun i occ -> if i = n then List.init m (fun j -> j :: occ) else [ occ ]) occs |> List.flatten

let assert_valid mat =
  let { arity; occs; bnds; pats; acts } = mat in
  let n = List.length pats in
  if List.length bnds <> n then failwith "bindings length mismatch";
  if List.length acts <> n then failwith "actions length mismatch";
  if List.length occs <> arity then failwith "occurrences length mismatch";
  List.iter (fun row -> if List.length row <> arity then failwith "row length mismatch") pats;
  mat

let spec_mat ctor col mat =
  let { arity; occs; bnds; pats; acts } = mat in
  let arity_map = List.fold_left (fun map pats -> check_arity map @@ List.nth pats col) StringMap.empty pats in
  let n = arity_of arity_map ctor in
  let new_arity = arity + n - 1 in
  let new_occs = unpack_nth occs col n in
  let new_pats, new_acts, new_bnds =
    unzip_map3
      (fun row act bnd ->
        match spec_row ctor n col occs row bnd with
        | Some (new_row, new_bnd) -> ([ new_row ], [ act ], [ new_bnd ])
        | None -> ([], [], []))
      pats acts bnds
  in
  let arity = new_arity in
  let occs = new_occs in
  let bnds = List.flatten new_bnds in
  let pats = List.flatten new_pats in
  let acts = List.flatten new_acts in
  assert_valid { arity; occs; bnds; pats; acts }

let is_trivial = function PAny | PVar _ -> true | _ -> false
let pat_identifier = function PVar x -> Some x | _ -> None

let default_cell pat occ =
  if is_trivial pat then
    match pat_identifier pat with Some name -> Some (OccurrenceMap.singleton occ name) | _ -> Some OccurrenceMap.empty
  else None

let default_row col occs row bnd =
  let cell_n = List.nth row col in
  let occ_n = List.nth occs col in
  let kept = default_cell cell_n occ_n in
  Option.map
    (fun new_bnd ->
      ( List.flatten @@ List.mapi (fun i cell -> if i = col then [] else [ cell ]) row,
        OccurrenceMap.union (fun _k _v1 v2 -> Some v2) bnd new_bnd ))
    kept

let default_mat col mat =
  let { arity; occs; bnds; pats; acts } = mat in
  let new_arity = arity - 1 in
  let new_occs = remove_at col occs in
  let new_pats, new_acts, new_bnds =
    unzip_map3
      (fun row act bnd ->
        match default_row col occs row bnd with
        | Some (new_row, new_bnd) -> ([ new_row ], [ act ], [ new_bnd ])
        | None -> ([], [], []))
      pats acts bnds
  in
  let arity = new_arity in
  let occs = new_occs in
  let bnds = List.flatten new_bnds in
  let pats = List.flatten new_pats in
  let acts = List.flatten new_acts in
  assert_valid { arity; occs; bnds; pats; acts }

let column_ctor_prefix_score { pats; _ } col =
  let score = ref 0 in
  List.iter
    (fun row ->
      let cell = List.nth row col in
      match cell with PApp (_, _) -> score := !score + 1 | _ -> ())
    pats;
  !score

module Hashtbl = Stdlib.Hashtbl

let column_small_branching_factor_score { pats; _ } col =
  let ctors = Hashtbl.create 8 in
  List.iter
    (fun row ->
      let cell = List.nth row col in
      let pat_desc = pat_desc cell in
      match pat_desc with PDCtor (c, _) -> Hashtbl.add ctors c () | _ -> ())
    pats;
  -Hashtbl.length ctors

let column_small_arity_score { pats; _ } col =
  let arity = ref 0 in
  List.iter
    (fun row ->
      let cell = List.nth row col in
      let pat_desc = pat_desc cell in
      match pat_desc with PDCtor (_, n) | PDTuple n -> arity := !arity + n | _ -> ())
    pats;
  - !arity

let choose_column m =
  let { arity; _ } = m in
  let heuristics = [ column_ctor_prefix_score; column_small_branching_factor_score; column_small_arity_score ] in
  let chosen = ref 0 in
  List.iter
    (fun h ->
      let max_score = ref 0 in
      for i = 0 to arity - 1 do
        let score = h m i in
        if score > !max_score then (
          max_score := score;
          chosen := i)
      done)
    heuristics;
  !chosen

(*
  e.g.
  match x with
  | P1 -> a1
  | P2 -> a2
  | P3 -> a3
  -->
  pats = [P1; P2; P3]
  acts = [a1; a2; a3]
*)
let make_mat pats acts =
  let arity = 1 in
  let occs = [ [] ] in
  let bnds = List.map (fun _ -> OccurrenceMap.empty) pats in
  let pats = List.map (fun pat -> [ pat ]) pats in
  { arity; occs; bnds; pats; acts }

let collect_pat_mat expr =
  let rec aux acc = function
    | Unit | Bool _ | Int _ | Float _ | Str _ | Builtin _ | Var _ | Ctor _ -> List.rev acc
    | App (f, args) ->
        let acc = aux acc f in
        List.fold_left aux acc args
    | Op (_, e1, e2) ->
        let acc = aux acc e1 in
        aux acc e2
    | Tup args -> List.fold_left aux acc args
    | Arr args -> List.fold_left aux acc args
    | Lam (_, e) -> aux acc e
    | Let (BSeq e1, e2) ->
        let acc = aux acc e1 in
        aux acc e2
    | Let (BOne (pat, e1), e2) ->
        let acc = aux acc e1 in
        let mat = make_mat [ pat ] [ 0 ] in
        let acc = mat :: acc in
        aux acc e2
    | Let (BRec bindings, e2) ->
        let acc =
          List.fold_left
            (fun acc (pat, e1) ->
              let mat = make_mat [ pat ] [ 0 ] in
              let acc = mat :: acc in
              aux acc e1)
            acc bindings
        in
        aux acc e2
    | Let (_, e2) -> aux acc e2
    | Sel (e, _) -> aux acc e
    | If (c, e1, e2) ->
        let acc = aux acc c in
        let acc = aux acc e1 in
        aux acc e2
    | Match (e, MatchPattern cases) ->
        let acc = aux acc e in
        let pats = List.map (fun (pat, _) -> pat) cases in
        let acts = List.mapi (fun i (_, _) -> i) cases in
        let arms = List.map (fun (_, arm) -> arm) cases in
        let mat = make_mat pats acts in
        List.fold_left aux (mat :: acc) arms
  in
  aux [] expr

let show_all_pattern_matrixes prog =
  let matrixes =
    List.fold_left
      (fun acc stmt ->
        match stmt with
        | Type _ -> acc
        | Fun (_, args_pats, e) ->
            let pats = List.map (fun pat -> make_mat [ pat ] [ 0 ]) args_pats in
            pats @ collect_pat_mat e @ acc
        | Term (None, e) -> collect_pat_mat e @ acc
        | Term (Some pat, e) ->
            let mat = make_mat [ pat ] [ 0 ] in
            collect_pat_mat e @ (mat :: acc))
      [] prog
  in
  PPrint.(
    separate (break 1)
    @@ List.map (fun mat -> string "pattern matrix:" ^^ break 1 ^^ nest 2 (pp_pattern_matrix mat)) matrixes)
