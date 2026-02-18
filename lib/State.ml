open Value
open Pattern
module Hashtbl = Core.Hashtbl

module Children = struct
  type 'a repr = Small of (int * 'a) list | Hash of (int, 'a) Hashtbl.t
  type 'a t = { mutable repr : 'a repr }

  let small_limit = 4
  let create () = { repr = Small [] }
  let length (t : 'a t) : int = match t.repr with Small lst -> List.length lst | Hash tbl -> Hashtbl.length tbl

  let find (t : 'a t) (key : int) : 'a option =
    match t.repr with Small lst -> List.assoc_opt key lst | Hash tbl -> Hashtbl.find tbl key

  let to_hash (lst : (int * 'a) list) : (int, 'a) Hashtbl.t =
    let tbl = Hashtbl.create (module Core.Int) in
    List.iter (fun (k, v) -> Hashtbl.set tbl ~key:k ~data:v) lst;
    tbl

  let promote_if_needed (t : 'a t) : unit =
    match t.repr with Small lst when List.length lst > small_limit -> t.repr <- Hash (to_hash lst) | _ -> ()

  let demote_if_needed (t : 'a t) : unit =
    match t.repr with
    | Hash tbl when Hashtbl.length tbl <= small_limit -> t.repr <- Small (Hashtbl.to_alist tbl)
    | _ -> ()

  let set (t : 'a t) (key : int) (data : 'a) : unit =
    match t.repr with
    | Small lst ->
        let lst' = (key, data) :: List.filter (fun (k, _) -> k <> key) lst in
        t.repr <- Small lst';
        promote_if_needed t
    | Hash tbl -> Hashtbl.set tbl ~key ~data

  let update (t : 'a t) (key : int) ~(f : 'a option -> 'a) : unit =
    let current = find t key in
    set t key (f current)

  let remove (t : 'a t) (key : int) : unit =
    match t.repr with
    | Small lst -> t.repr <- Small (List.filter (fun (k, _) -> k <> key) lst)
    | Hash tbl ->
        Hashtbl.remove tbl key;
        demote_if_needed t

  let iter (t : 'a t) ~(f : 'a -> unit) : unit =
    match t.repr with Small lst -> List.iter (fun (_, v) -> f v) lst | Hash tbl -> Hashtbl.iter tbl ~f
end

type env = value Dynarray.t

(* Notes on the control representation are in docs/internal.md#cek-state-representation-stateml. *)
and exp = {
  (* One step transition. Throw an exception when done. *)
  step : world -> unit;
  (*pc is an isomorphism to func, and pc -> func is a table lookup.*)
  pc : int;
}

and kont = value
and 'a cek = { mutable c : exp; mutable e : 'a Dynarray.t; mutable k : 'a }
and state = value cek
and step = { src : pattern cek; dst : value cek; sc : int; mutable hit : int; mutable insert_time : int }
and memo = trie option Array.t
and trie = { mutable steps : step option; mutable var : trie option; const : trie Children.t }
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
  { c; e; k }

(*the order is not fixed. use this for AC stuff*)
let fold_ek (s : 'a cek) (acc : 'acc) (f : 'acc -> 'a -> 'acc) : 'acc =
  let acc = Dynarray.fold_left (fun acc v -> f acc v) acc s.e in
  f acc s.k

let zip_ek (x : 'a cek) (y : 'b cek) : ('a * 'b) cek option =
  if Dynarray.length x.e != Dynarray.length y.e then None
  else (
    assert (Dynarray.length x.e = Dynarray.length y.e);
    let c = x.c in
    assert (x.c.pc = y.c.pc);
    let e = Dynarray.init (Dynarray.length x.e) (fun i -> (Dynarray.get x.e i, Dynarray.get y.e i)) in
    let k = (x.k, y.k) in
    Some { c; e; k })

let zipwith_ek (f : 'a -> 'b -> 'c) (x : 'a cek) (y : 'b cek) : 'c cek =
  assert (Dynarray.length x.e = Dynarray.length y.e);
  let c = x.c in
  assert (x.c.pc = y.c.pc);
  let e = Dynarray.init (Dynarray.length x.e) (fun i -> f (Dynarray.get x.e i) (Dynarray.get y.e i)) in
  let k = f x.k y.k in
  { c; e; k }

let map_ek (f : 'a -> 'b) (s : 'a cek) : 'b cek =
  let c = s.c in
  let e = Dynarray.map f s.e in
  let k = f s.k in
  { c; e; k }

let maps_ek (f : 'a -> source -> 'b) (s : 'a cek) : 'b cek =
  let c = s.c in
  let e = Dynarray.mapi (fun i v -> f v (Source.E i)) s.e in
  let k = f s.k Source.K in
  { c; e; k }

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
  match (e, s.k) with Some e, Some v -> Some { c; e; k = v } | _ -> None

let string_of_cek (s : state) : string =
  "pc: " ^ string_of_int s.c.pc
  ^ (", e: " ^ (Dynarray.to_list s.e |> List.map string_of_value |> String.concat ", "))
  ^ ", k: " ^ string_of_value s.k

let string_of_cek_generic (string_of_a : 'a -> string) (s : 'a cek) : string =
  "pc: " ^ string_of_int s.c.pc
  ^ (", e: " ^ (Dynarray.to_list s.e |> List.map string_of_a |> String.concat ", "))
  ^ ", k: " ^ string_of_a s.k

let is_done (s : state) : bool = match s.k with Node (ConstructorTag ct, _) when ct = 0 -> s.c.pc = 0 | _ -> false

(* The continuation is stored last since it is mainly meaningful together with the environment. *)
let ek_to_list (s : 'a cek) : 'a list =
  let lst = Dynarray.to_list s.e in
  lst @ [ s.k ]
