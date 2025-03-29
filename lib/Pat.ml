open Syntax
open Map

let compile ast = ast

type occurrence = int list

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

type patdesc = PDInt of int | PDBool of bool | PDUnit | PDCtor of string * int | PDTuple of int

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

let[@tail_mod_cons] rec unzip_map3 f l1 l2 l3 =
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
  | _, _, _ -> invalid_arg "map3"

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

