type ordering = Lt | Eq | Gt
type 'a ord = { cmp : 'a -> 'a -> ordering }
type color = Red | Black
type ('k, 'v) t = Leaf | Node of color * ('k, 'v) t * 'k * 'v * ('k, 'v) t

let empty = Leaf
let node c l k v r = Node (c, l, k, v, r)

let rec depth (f : int -> int -> int) n =
  match n with Leaf -> 0 | Node (_, l, _, _, r) -> f (depth f l) (depth f r) + 1

let rec min n : ('k * 'v) option =
  match n with Leaf -> None | Node (_, Leaf, k, v, _) -> Some (k, v) | Node (_, l, _, _, _) -> min l

let rec max n : ('k * 'v) option =
  match n with Leaf -> None | Node (_, _, k, v, Leaf) -> Some (k, v) | Node (_, _, _, _, r) -> max r

let rec fold_left (f : 's -> 'k -> 'v -> 's) (init : 's) (n : ('k, 'v) t) : 's =
  match n with Leaf -> init | Node (_, l, k, v, r) -> fold_left f (f (fold_left f init l) k v) r

let rec fold_right (f : 's -> 'k -> 'v -> 's) (init : 's) (n : ('k, 'v) t) : 's =
  match n with Leaf -> init | Node (_, l, k, v, r) -> fold_right f (f (fold_right f init r) k v) l

let singleton (k : 'k) (v : 'v) : ('k, 'v) t = Node (Red, Leaf, k, v, Leaf)

let balance1 (n : ('k, 'v) t) (kz : 'k) (vz : 'v) (d : ('k, 'v) t) =
  match n with
  | Node (Red, Node (Red, a, kx, vx, b), ky, vy, c) | Node (Red, a, kx, vx, Node (Red, b, ky, vy, c)) ->
      node Red (node Black a kx vx b) ky vy (node Black c kz vz d)
  | a -> Node (Black, a, kz, vz, d)

let balance2 (a : ('k, 'v) t) (kx : 'k) (vx : 'v) (d : ('k, 'v) t) =
  match d with
  | Node (Red, Node (Red, b, ky, vy, c), kz, vz, d) | Node (Red, b, ky, vy, Node (Red, c, kz, vz, d)) ->
      node Red (node Black a kx vx b) ky vy (node Black c kz vz d)
  | b -> Node (Black, a, kx, vx, b)

let is_red (node : ('k, 'v) t) : bool = match node with Node (Red, _, _, _, _) -> true | _ -> false
let is_black (node : ('k, 'v) t) : bool = match node with Node (Black, _, _, _, _) -> true | _ -> false

let rec ins ~(ord : 'k ord) ~replace (n : ('k, 'v) t) (kx : 'k) (vx : 'v) : ('k, 'v) t * _ =
  match n with
  | Leaf -> (Node (Red, Leaf, kx, vx, Leaf), `Ok)
  | Node (Red, a, ky, vy, b) -> (
      match ord.cmp kx ky with
      | Lt ->
          let r, s = ins ~ord ~replace a kx vx in
          (Node (Red, r, ky, vy, b), s)
      | Gt ->
          let r, s = ins ~ord ~replace b kx vx in
          (Node (Red, a, ky, vy, r), s)
      | Eq -> if replace then (Node (Red, a, kx, vx, b), `Duplicate) else (Node (Red, a, ky, vy, b), `Duplicate))
  | Node (Black, a, ky, vy, b) -> (
      match ord.cmp kx ky with
      | Lt ->
          let r, s = ins ~ord ~replace a kx vx in
          (balance1 r ky vy b, s)
      | Gt ->
          let r, s = ins ~ord ~replace b kx vx in
          (balance2 a ky vy r, s)
      | Eq -> if replace then (Node (Black, a, kx, vx, b), `Duplicate) else (Node (Black, a, ky, vy, b), `Duplicate))

(* let rec ins ~(ord : 'k ord) (n : ('k, 'v) t) (kx : 'k) (vx : 'v) : ('k, 'v) t * bool =
   let flag = ref false in
   let r = ins_core ~ord ~flag n kx vx in
   (r, !flag) *)
let set_black (node : ('k, 'v) t) = match node with Node (_, l, k, v, r) -> Node (Black, l, k, v, r) | e -> e
let set_red (node : ('k, 'v) t) = match node with Node (_, l, k, v, r) -> Node (Red, l, k, v, r) | e -> e

let add ~ord (n : ('k, 'v) t) (k : 'k) (v : 'v) : ('k, 'v) t * _ =
  if is_red n then
    let r, s = ins ~ord ~replace:false n k v in
    match s with `Ok -> (set_black r, s) | `Duplicate -> (n, s)
  else ins ~ord ~replace:false n k v

exception KeyDup

let add_exn ~ord n k v =
  if is_red n then
    let r, s = ins ~ord ~replace:false n k v in
    match s with `Ok -> set_black r | `Duplicate -> raise KeyDup
  else Stdlib.fst (ins ~ord ~replace:false n k v)

let set ~ord n k v =
  if is_red n then
    let r, s = ins ~ord ~replace:true n k v in
    match s with `Ok -> set_black r | `Duplicate -> r
  else Stdlib.fst (ins ~ord ~replace:true n k v)

let bal_left l k v r =
  match (l, k, v, r) with
  | Node (Red, a, kx, vx, b), k, v, r -> node Red (node Black a kx vx b) k v r
  | l, k, v, Node (Black, a, ky, vy, b) -> balance2 l k v (node Red a ky vy b)
  | l, k, v, Node (Red, Node (Black, a, ky, vy, b), kz, vz, c) ->
      node Red (node Black l k v a) ky vy (balance2 b kz vz (set_red c))
  | l, k, v, r -> node Red l k v r (* unreachable *)

let bal_right l k v r =
  match r with
  | Node (Red, b, ky, vy, c) -> node Red l k v (node Black b ky vy c)
  | _ -> (
      match l with
      | Node (Black, a, kx, vx, b) -> balance1 (node Red a kx vx b) k v r
      | Node (Red, a, kx, vx, Node (Black, b, ky, vy, c)) ->
          node Red (balance1 (set_red a) kx vx b) ky vy (node Black c k v r)
      | _ -> node Red l k v r)

let rec size t = match t with Leaf -> 0 | Node (_, x, _, _, y) -> size x + size y + 1

let rec append_trees (l : ('k, 'v) t) (r : ('k, 'v) t) : ('k, 'v) t =
  match (l, r) with
  | Leaf, x -> x
  | x, Leaf -> x
  | Node (Red, a, kx, vx, b), Node (Red, c, ky, vy, d) -> (
      match append_trees b c with
      | Node (Red, b', kz, vz, c') -> node Red (node Red a kx vx b') kz vz (node Red c' ky vy d)
      | bc -> node Red a kx vx (node Red bc ky vy d))
  | Node (Black, a, kx, vx, b), Node (Black, c, ky, vy, d) -> (
      match append_trees b c with
      | Node (Red, b', kz, vz, c') -> node Red (node Black a kx vx b') kz vz (node Black c' ky vy d)
      | bc -> bal_left a kx vx (node Black bc ky vy d))
  | a, Node (Red, b, kx, vx, c) -> node Red (append_trees a b) kx vx c
  | Node (Red, a, kx, vx, b), c -> node Red a kx vx (append_trees b c)

let rec delete ~ord x t =
  match t with
  | Leaf -> Leaf
  | Node (_, a, y, v, b) -> (
      match ord.cmp x y with
      | Lt -> if is_black a then bal_left (delete ~ord x a) y v b else node Red (delete ~ord x a) y v b
      | Gt -> if is_black b then bal_right a y v (delete ~ord x b) else node Red a y v (delete ~ord x b)
      | Eq -> append_trees a b)

let erase ~ord x t =
  let t = delete ~ord x t in
  set_black t

let rec all (p : 'k -> 'v -> bool) n : bool =
  match n with Leaf -> true | Node (_, l, k, v, r) -> p k v && all p l && all p r

let rec any (p : 'k -> 'v -> bool) n : bool =
  match n with Leaf -> false | Node (_, l, k, v, r) -> p k v || any p l || any p r

let rec find_core ~ord t x : ('k * 'v) option =
  match t with
  | Leaf -> None
  | Node (_, a, ky, vy, b) -> (
      match ord.cmp x ky with Lt -> find_core ~ord a x | Gt -> find_core ~ord b x | Eq -> Some (ky, vy))

let rec find ~ord t x : 'v option =
  match t with
  | Leaf -> None
  | Node (_, a, ky, vy, b) -> ( match ord.cmp x ky with Lt -> find ~ord a x | Gt -> find ~ord b x | Eq -> Some vy)

let rec lower_bound ~ord n x lb : ('k * 'v) option =
  match (n, x, lb) with
  | Leaf, _, lb -> lb
  | Node (_, a, ky, vy, b), x, lb -> (
      match ord.cmp x ky with
      | Lt -> lower_bound ~ord a x lb
      | Gt -> lower_bound ~ord b x (Some (ky, vy))
      | Eq -> Some (ky, vy))

let rec map (f : 'k -> 'a -> 'b) (n : ('k, 'a) t) : ('k, 'b) t =
  match n with
  | Leaf -> Leaf
  | Node (color, lchild, key, value, rchild) -> node color (map f lchild) key (f key value) (map f rchild)

let iteri (n : ('k, 'a) t) ~(f : 'k -> 'a -> unit) : unit = ignore (map f n)
let to_list n = fold_right (fun xs k v -> (k, v) :: xs) [] n
(* let of_list ~ord xs = List.fold_left (fun n (k, v) -> set ~ord n k v) empty xs *)

let find_exn ~ord n k = match find ~ord n k with None -> raise Not_found | Some v -> v
let string_of_color = function Red -> "R" | Black -> "B"

let rec pp ?(kpp = fun _ -> "<key>") ?(vpp = fun _ -> "<val>") ?(indent = 0) (node : ('k, 'v) t) : unit =
  let pad = String.make indent ' ' in
  match node with
  | Leaf -> Printf.printf "%sLeaf\n" pad
  | Node (c, l, k, v, r) ->
      Printf.printf "%sNode(%s) k=%s v=%s\n" pad (string_of_color c) (kpp k) (vpp v);
      pp ~kpp ~vpp ~indent:(indent + 2) l;
      pp ~kpp ~vpp ~indent:(indent + 2) r

let to_string ?(kpp = fun _ -> "<key>") ?(vpp = fun _ -> "<val>") (node : ('k, 'v) t) : string =
  let b = Buffer.create 256 in
  let rec aux indent node =
    let pad = String.make indent ' ' in
    match node with
    | Leaf -> Buffer.add_string b (pad ^ "Leaf\n")
    | Node (c, l, k, v, r) ->
        Buffer.add_string b (Printf.sprintf "%sNode(%s) k=%s v=%s\n" pad (string_of_color c) (kpp k) (vpp v));
        aux (indent + 2) l;
        aux (indent + 2) r
  in
  aux 0 node;
  Buffer.contents b
