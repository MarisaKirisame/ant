open Value
open Pattern
open BatFingerTree
module Hashtbl = Core.Hashtbl

type env = value Dynarray.t

(* Notes on the control representation are in docs/internal.md#cek-state-representation-stateml. *)
and exp = {
  (* One step transition. Throw an exception when done. *)
  step : world -> unit;
  (*pc is an isomorphism to func, and pc -> func is a table lookup.*)
  pc : int;
}

and kont = value

and 'a cek = {
  mutable c : exp;
  mutable e : 'a Dynarray.t;
  mutable k : 'a;
  (* step_count *)
  mutable sc : int;
}

and state = value cek
and step = { src : pattern cek; dst : value cek; sc : int }
and memo = step list ref
and world = { state : state; memo : memo; resolved : bool cek }

let cek_get (cek : 'a cek) (src : Source.t) : 'a =
  match src with
  | Source.E i ->
      assert (i < Dynarray.length cek.e);
      Dynarray.get cek.e i
  | Source.K -> cek.k

let copy_state s : state =
  let c = s.c in
  let e = Dynarray.map (fun v -> v) s.e in
  let k = s.k in
  let sc = s.sc in
  { c; e; k; sc }

(*the order is not fixed. use this for AC stuff*)
let fold_ek (s : 'a cek) (acc : 'acc) (f : 'acc -> 'a -> 'acc) : 'acc =
  let acc = Dynarray.fold_left (fun acc v -> f acc v) acc s.e in
  f acc s.k

let zip_ek (s1 : 'a cek) (s2 : 'b cek) : ('a * 'b) cek option =
  if Dynarray.length s1.e != Dynarray.length s2.e then None
  else (
    assert (Dynarray.length s1.e = Dynarray.length s2.e);
    let c = s1.c in
    let e = Dynarray.init (Dynarray.length s1.e) (fun i -> (Dynarray.get s1.e i, Dynarray.get s2.e i)) in
    let k = (s1.k, s2.k) in
    let sc = s1.sc in
    Some { c; e; k; sc })

let map_ek (f : 'a -> 'b) (s : 'a cek) : 'b cek =
  let c = s.c in
  let e = Dynarray.map f s.e in
  let k = f s.k in
  let sc = s.sc in
  { c; e; k; sc }

let maps_ek (f : 'a -> source -> 'b) (s : 'a cek) : 'b cek =
  let c = s.c in
  let e = Dynarray.mapi (fun i v -> f v (Source.E i)) s.e in
  let k = f s.k Source.K in
  let sc = s.sc in
  { c; e; k; sc }

let make_world state memo : world = { state; memo; resolved = map_ek (fun _ -> false) state }

let rec option_list_to_list_option (lst : 'a option list) : 'a list option =
  match lst with
  | [] -> Some []
  | x :: xs -> (
      match x with
      | Some v -> ( match option_list_to_list_option xs with Some vs -> Some (v :: vs) | None -> None)
      | _ -> None)

let option_ek_to_ek_option (s : 'a option cek) : 'a cek option =
  let c = s.c in
  let e =
    let lst = Dynarray.to_list s.e in
    match option_list_to_list_option lst with Some vs -> Some (Dynarray.of_list vs) | None -> None
  in
  match (e, s.k) with Some e, Some v -> Some { c; e; k = v; sc = s.sc } | _ -> None

let string_of_cek (s : state) : string =
  assert (s.sc <= 10000);
  "pc: " ^ string_of_int s.c.pc
  ^ (", e: " ^ (Dynarray.to_list s.e |> List.map string_of_value |> String.concat ", "))
  ^ ", k: " ^ string_of_value s.k ^ ", sc: " ^ string_of_int s.sc

let is_done (s : state) : bool =
  match Generic.front_exn s.k ~monoid:Value.monoid ~measure:Value.measure with
  | _, Word w -> ( match w with ConstructorTag ct when ct = 0 -> s.c.pc = 0 | _ -> false)
  | _ -> failwith "unreachable"
