open Common
open PPrint
open Syntax
open Memo
open State
open Code

(*todo: implement tail call*)
(*todo: do not do a stack machine*)

(* As K come with interpretative overhead,
 *   we want to use K as little as possible,
 *   instead storing the computed temporary variables onto the env as a stack,
 *   only using K whenever we do a non-tail function call.
 *)
(* The following is almost a reimplementation of Lean 4's RBMap *)

type ordering = Lt | Eq | Gt
type 'a ord = { cmp : 'a -> 'a -> ordering }

module RBMap = struct
  type color = Red | Black
  type ('k, 'v) t = Leaf | Node of color * ('k, 'v) t * 'k * 'v * ('k, 'v) t

  let empty = Leaf
  let node c l k v r = Node (c, l, k, v, r)

  let rec depth (f : int -> int -> int) n =
    match n with
    | Leaf -> 0
    | Node (_, l, _, _, r) -> (f (depth f l) (depth f r)) + 1

  let rec min n : ('k * 'v) option =
    match n with
    | Leaf -> None
    | Node (_, Leaf, k, v, _) -> Some (k, v)
    | Node (_, l, _, _, _) -> min l
  let rec max n : ('k * 'v) option =
    match n with
    | Leaf -> None
    | Node (_, _, k, v, Leaf) -> Some (k, v)
    | Node (_, _, _, _, r) -> max r
  let rec fold_left (f : 's -> 'k -> 'v -> 's) (init : 's) (n : ('k, 'v) t) : 's =
    match n with
    | Leaf -> init
    | Node (_, l, k, v, r) -> fold_left f (f (fold_left f init l) k v) r
  let rec fold_right (f : 's -> 'k -> 'v -> 's) (init : 's) (n : ('k, 'v) t) : 's =
    match n with
    | Leaf -> init
    | Node (_, l, k, v, r) -> fold_right f (f (fold_right f init r) k v) l
  let singleton (k : 'k) (v : 'v) : ('k, 'v) t =
    Node (Red, Leaf, k, v, Leaf)
  let balance1 (n : ('k, 'v) t) (kz : 'k) (vz : 'v) (d : ('k, 'v) t) =
    match n with
    | Node (Red, Node (Red, a, kx, vx, b), ky, vy, c)
    | Node (Red, a, kx, vx, Node (Red, b, ky, vy, c)) -> node Red (node Black a kx vx b) ky vy (node Black c kz vz d)
    | a -> Node (Black, a, kz, vz, d)
  let balance2 (a : ('k, 'v) t) (kx : 'k) (vx : 'v) (d : ('k, 'v) t) =
    match d with
    | Node (Red, Node (Red, b, ky, vy, c), kz, vz, d)
    | Node (Red, b, ky, vy, Node (Red, c, kz, vz, d)) -> node Red (node Black a kx vx b) ky vy (node Black c kz vz d)
    | b -> Node (Black, a, kx, vx, b)
  let is_red (node : ('k, 'v) t) : bool =
    match node with
    | Node (Red, _, _, _, _) -> true
    | _ -> false
  let is_black (node : ('k, 'v) t) : bool =
    match node with
    | Node (Black, _, _, _, _) -> true
    | _ -> false
  let rec ins ~(ord : 'k ord) ~replace (n : ('k, 'v) t) (kx : 'k) (vx : 'v) : ('k, 'v) t * _ =
    match n with
    | Leaf ->
      (Node (Red, Leaf, kx, vx, Leaf), `Ok)
    | Node (Red, a, ky, vy, b) ->
      (match ord.cmp kx ky with
      | Lt ->
        let (r, s) = ins ~ord ~replace a kx vx in
        (Node (Red, r, ky, vy, b), s)
      | Gt ->
        let (r, s) = (ins ~ord ~replace b kx vx) in
        (Node (Red, a, ky, vy, r), s)
      | Eq ->
        if replace then
          (Node (Red, a, kx, vx, b), `Duplicate)
        else
          (Node (Red, a, ky, vy, b), `Duplicate))
    | Node (Black, a, ky, vy, b) ->
      match ord.cmp kx ky with
      | Lt ->
        let (r, s) = ins ~ord ~replace a kx vx in
        (balance1 r ky vy b, s)
      | Gt ->
        let (r, s) = ins ~ord ~replace b kx vx in
        (balance2 a ky vy r, s)
      | Eq -> 
        if replace then
          (Node (Black, a, kx, vx, b), `Duplicate)
        else
          (Node (Black, a, ky, vy, b), `Duplicate)
  (* let rec ins ~(ord : 'k ord) (n : ('k, 'v) t) (kx : 'k) (vx : 'v) : ('k, 'v) t * bool =
    let flag = ref false in
    let r = ins_core ~ord ~flag n kx vx in
    (r, !flag) *)
  let set_black (node : ('k, 'v) t) =
    match node with
    | Node (_, l, k, v, r) -> Node (Black, l, k, v, r)
    | e -> e
  let set_red (node : ('k, 'v) t) =
    match node with
    | Node (_, l, k, v, r) -> Node (Red, l, k, v, r)
    | e -> e
  let add ~ord (n : ('k, 'v) t) (k : 'k) (v : 'v) : ('k, 'v) t * _ =
    if is_red n then
      let (r, s) = ins ~ord ~replace:false n k v in
      match s with
      | `Ok -> (set_black r, s)
      | `Duplicate -> (n, s)
    else
      ins ~ord ~replace:false n k v
  exception KeyDup
  let add_exn ~ord n k v =
    if is_red n then
      let (r, s) = ins ~ord ~replace:false n k v in
      match s with
      | `Ok -> set_black r
      | `Duplicate -> raise KeyDup
    else
      Stdlib.fst (ins ~ord ~replace:false n k v)
  let set ~ord n k v =
    if is_red n then
      let (r, s) = ins ~ord ~replace:true n k v in
      match s with
      | `Ok -> set_black r
      | `Duplicate -> r
    else
      Stdlib.fst (ins ~ord ~replace:true n k v)
  let bal_left l k v r =
    match l, k, v, r with
    | Node (Red, a, kx, vx, b), k, v, r -> node Red (node Black a kx vx b) k v r
    | l, k, v, Node (Black, a, ky, vy, b) -> balance2 l k v (node Red a ky vy b)
    | l, k, v, Node (Red, (Node (Black, a, ky, vy, b)), kz, vz, c) -> node Red (node Black l k v a) ky vy (balance2 b kz vz (set_red c))
    | l, k, v, r -> node Red l k v r (* unreachable *)
  let bal_right l k v r =
    match r with
    | Node (Red, b, ky, vy, c) -> node Red l k v (node Black b ky vy c)
    | _ ->
      match l with
      | Node (Black, a, kx, vx, b) -> balance1 (node Red a kx vx b) k v r
      | Node (Red, a, kx, vx, Node (Black, b, ky, vy, c)) -> node Red (balance1 (set_red a) kx vx b) ky vy (node Black c k v r)
      | _ -> node Red l k v r
  (** number of elements *)
  let rec size t =
    match t with
    | Leaf -> 0
    | Node (_, x, _, _, y) -> size x + size y + 1
  let rec append_trees (l : ('k, 'v) t) (r : ('k, 'v) t) : ('k, 'v) t =
    match l, r with
    | Leaf, x -> x
    | x, Leaf -> x
    | Node (Red, a, kx, vx, b), Node (Red, c, ky, vy, d) ->
      (match append_trees b c with
      | Node (Red, b', kz, vz, c') -> node Red (node Red a kx vx b') kz vz (node Red c' ky vy d)
      | bc -> node Red a kx vx (node Red bc ky vy d))
    | Node (Black, a, kx, vx, b), Node (Black, c, ky, vy, d) ->
      (match append_trees b c with
      | Node (Red, b', kz, vz, c') -> node Red (node Black a kx vx b') kz vz (node Black c' ky vy d)
      | bc -> bal_left a kx vx (node Black bc ky vy d))
    | a, Node (Red, b, kx, vx, c) -> node Red (append_trees a b) kx vx c
    | Node (Red, a, kx, vx, b), c -> node Red a kx vx (append_trees b c)
  let rec delete ~ord x t =
    match t with
    | Leaf -> Leaf
    | Node (_, a, y, v, b) ->
      match ord.cmp x y with
      | Lt ->
        if is_black a then bal_left (delete ~ord x a) y v b
        else node Red (delete ~ord x a) y v b
      | Gt ->
        if is_black b then bal_right a y v (delete ~ord x b)
        else node Red a y v (delete ~ord x b)
      | Eq -> append_trees a b
  let erase ~ord x t = let t = delete ~ord x t in set_black t

  let rec all (p : 'k -> 'v -> bool) n : bool =
    match n with
    | Leaf -> true
    | Node (_, l, k, v, r) -> p k v && all p l && all p r
  let rec any (p : 'k -> 'v -> bool) n : bool =
    match n with
    | Leaf -> false
    | Node (_, l, k, v, r) -> p k v || any p l || any p r

  let rec find_core ~ord t x : ('k * 'v) option =
    match t with
    | Leaf -> None
    | Node (_, a, ky, vy, b) ->
      match ord.cmp x ky with
      | Lt -> find_core ~ord a x
      | Gt -> find_core ~ord b x
      | Eq -> Some (ky, vy)
  let rec find ~ord t x : 'v option =
    match t with
    | Leaf -> None
    | Node (_, a, ky, vy, b) ->
      match ord.cmp x ky with
      | Lt -> find ~ord a x
      | Gt -> find ~ord b x
      | Eq -> Some vy
  let rec lower_bound ~ord n x lb : ('k * 'v) option =
    match n, x, lb with
    | Leaf, _, lb -> lb
    | Node (_, a, ky, vy, b), x, lb ->
      match ord.cmp x ky with
      | Lt -> lower_bound ~ord a x lb
      | Gt -> lower_bound ~ord b x (Some (ky, vy))
      | Eq -> Some (ky, vy)
  let rec map (f : 'k -> 'a -> 'b) (n : ('k, 'a) t) : ('k, 'b) t =
    match n with
    | Leaf -> Leaf
    | Node (color, lchild, key, value, rchild) -> node color (map f lchild) key (f key value) (map f rchild)

  let iteri (n : ('k, 'a) t) ~(f : 'k -> 'a -> unit) : unit = ignore (map f n)

  let to_list n = fold_right (fun xs k v -> (k, v) :: xs) [] n
  (* let of_list ~ord xs = List.fold_left (fun n (k, v) -> set ~ord n k v) empty xs *)

  let find_exn ~ord n k =
    match find ~ord n k with
    | None -> raise Not_found
    | Some v -> v

  let string_of_color = function Red -> "R" | Black -> "B"

  let rec pp ?(kpp = (fun _ -> "<key>")) ?(vpp = (fun _ -> "<val>")) ?(indent = 0) (node : ('k, 'v) t) : unit =
    let pad = String.make indent ' ' in
    match node with
    | Leaf -> Printf.printf "%sLeaf\n" pad
    | Node (c, l, k, v, r) ->
      Printf.printf "%sNode(%s) k=%s v=%s\n" pad (string_of_color c) (kpp k) (vpp v);
      pp ~kpp ~vpp ~indent:(indent + 2) l;
      pp ~kpp ~vpp ~indent:(indent + 2) r

  let to_string ?(kpp = (fun _ -> "<key>")) ?(vpp = (fun _ -> "<val>")) (node : ('k, 'v) t) : string =
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
end

module RBTree = struct

  type 'a t = ('a, unit) RBMap.t

  let empty : 'a t = RBMap.empty

  let singleton x : 'a t = RBMap.singleton x ()

  let add ~ord (n : 'a t) (k : 'k) : 'a t * _ =
    RBMap.add ~ord n k ()

  let add_exn ~ord (n : 'a t) (k : 'k) : 'a t =
    RBMap.add_exn ~ord n k ()

  let set ~ord (n : 'a t) (k : 'k) : 'a t =
    RBMap.set ~ord n k ()

  let erase ~ord x (n : 'a t) : 'a t = RBMap.erase ~ord x n
  
  let all (p : 'a -> bool) (n : 'a t) = RBMap.all (fun k _ -> p k) n

  let any (p : 'a -> bool) (n : 'a t) = RBMap.any (fun k _ -> p k) n

  let fold_left (f : 's -> 'a -> 's) (init : 's) (n : 'a t) : 's =
    RBMap.fold_left (fun s k v -> f s k) init n
  
  let fold_right (f : 's -> 'a -> 's) (init : 's) (n : 'a t) : 's =
    RBMap.fold_right (fun s k v -> f s k) init n

  let depth (f : int -> int -> int) (n : 'a t) = RBMap.depth f n
  let size (n : 'a t) = RBMap.size n

  let union (a : 'a t) (b : 'a t) : 'a t = RBMap.append_trees a b

  let contains ~ord (t : 'a t) x : bool = Option.is_some (RBMap.find ~ord t x)

  let to_list (n : 'a t) = Stdlib.List.map Stdlib.fst (RBMap.to_list n)
  (* let of_list ~ord xs : 'a t = RBMap.of_list ~ord (List.map (fun x -> (x, ())) xs) *)

  let iteri (n : 'a t) ~(f : 'a -> unit) : unit = ignore (RBMap.map (fun k _ -> f k) n)

end

let ord_str : string ord = { cmp = fun x y -> if x < y then Lt else if x > y then Gt else Eq }
let ord_int : int ord = { cmp = fun x y -> if x < y then Lt else if x > y then Gt else Eq }

type ctx = {
  arity : (string, int) Hashtbl.t;
  ctag : (string, int) Hashtbl.t;
  constructor_degree : int Dynarray.t;
  conts : (string * (world code -> words code -> unit code)) Dynarray.t;
  mutable conts_count : int;
  func_pc : (string, int) Hashtbl.t;
}

let add_cont (ctx : ctx) (name : string) (arity : int) (app : world code -> words code -> unit code) : unit =
  Hashtbl.add_exn ctx.arity ~key:name ~data:arity;
  Hashtbl.add_exn ctx.ctag ~key:name ~data:(Hashtbl.length ctx.ctag);
  Dynarray.add_last ctx.constructor_degree (1 - arity);
  Dynarray.add_last ctx.conts (name, app);
  ctx.conts_count <- ctx.conts_count + 1

let new_ctx () : ctx =
  let ctx =
    {
      arity = Hashtbl.create (module Core.String);
      ctag = Hashtbl.create (module Core.String);
      constructor_degree = Dynarray.create ();
      conts = Dynarray.create ();
      conts_count = 0;
      func_pc = Hashtbl.create (module Core.String);
    }
  in
  add_cont ctx "cont_done" 0 (fun w _ -> exec_done w);
  ctx

let codes : (world -> unit) code option Dynarray.t = Dynarray.create ()

type pc = int

let add_code (c : (world -> unit) code option) : pc =
  let pc = Dynarray.length codes in
  Dynarray.add_last codes c;
  pc

let set_code (i : int) (c : (world -> unit) code) : unit = Dynarray.set codes i (Some c)

let add_code_k (k : pc -> (world -> unit) code * 'a) : 'a =
  let pc = add_code None in
  let code, ret = k pc in
  set_code pc code;
  ret

let ant_pp_ocaml_adt adt_name ctors =
  string
    ("type ocaml_" ^ adt_name ^ " = "
    ^ String.concat " | "
        (List.map
           (fun (con_name, types) ->
             if List.length types = 0 then con_name
             else con_name ^ " of " ^ String.concat " * " (List.map (fun _ -> "Value.seq") types))
           ctors))

let ant_pp_adt_constructors (e : ctx) adt_name ctors =
  separate_map (break 1)
    (fun (con_name, types) ->
      Hashtbl.add_exn ~key:con_name ~data:(List.length types) e.arity;
      Hashtbl.add_exn ~key:con_name ~data:(Hashtbl.length e.ctag) e.ctag;
      Dynarray.add_last e.constructor_degree (1 - List.length types);
      let param_names = List.mapi (fun i _ -> "x" ^ string_of_int i) types in
      let param_docs = List.map string param_names in
      let param_codes : Value.seq code list = List.map (fun name -> code (string name)) param_names in
      let ctor_tag = int (Hashtbl.find_exn e.ctag con_name) in
      let body = memo_appends (from_constructor ctor_tag :: param_codes) in
      string "let "
      ^^ string (adt_name ^ "_" ^ con_name)
      ^^ (if param_docs = [] then empty else space ^^ separate space param_docs)
      ^^ string ": Value.seq = "
      ^^ uncode body)
    ctors

(*todo: distinguish ffi inner type.*)
let ant_pp_adt_ffi e adt_name ctors =
  string
    ("let from_ocaml_" ^ adt_name ^ " x = match x with | "
    ^ String.concat " | "
        (List.map
           (fun (con_name, types) ->
             let args = List.mapi (fun i _ -> "x" ^ string_of_int i) types in
             (if List.length types = 0 then con_name else con_name ^ "(" ^ String.concat ", " args ^ ")")
             ^ " -> " ^ adt_name ^ "_" ^ con_name ^ " " ^ String.concat " " args)
           ctors))
  ^^ break 1
  ^^ (
      let head =
        string
          ("let to_ocaml_" ^ adt_name
         ^ " x = let (h, t) = Option.get (Memo.list_match x) in match ")
        ^^ uncode (word_get_value (code $ string "h"))
        ^^ string " with | "
      in
      let cases =
        String.concat " | "
          (List.map
             (fun (con_name, types) ->
               string_of_int (Hashtbl.find_exn e.ctag con_name)
               ^ " -> "
               ^
               if List.length types = 0 then con_name
               else
                 "let ["
                 ^ String.concat ";" (List.mapi (fun i _ -> "x" ^ string_of_int i) types)
                 ^ "] = Memo.splits t in " ^ con_name ^ "("
                 ^ String.concat "," (List.mapi (fun i _ -> "x" ^ string_of_int i) types)
                 ^ ")")
             ctors)
      in
      head ^^ string cases)

let ant_pp_adt (e : ctx) adt_name ctors =
  let generate_ocaml_adt = ant_pp_ocaml_adt adt_name ctors in
  let generate_adt_constructors = ant_pp_adt_constructors e adt_name ctors in
  generate_ocaml_adt ^^ break 1 ^^ generate_adt_constructors ^^ break 1 ^^ ant_pp_adt_ffi e adt_name ctors

let apply_cont : pc = add_code None

type env = (string, int) RBMap.t

let new_env () : env = RBMap.empty

type scope = {
  meta_env : (string, int option) RBMap.t linear;
  (*Note: env_length is not the amount of entries in meta_env above! It is the length of the environment when executing the cek machine.*)
  env_length : int;
  progressed : bool;
}

let new_scope () = { meta_env = make_linear RBMap.empty; env_length = 0; progressed = false }
let push_s s = { s with env_length = s.env_length + 1; progressed = true }

let extend_s s name =
  let meta_env = write_linear s.meta_env in

  (* Hashtbl.add_exn meta_env ~key:name ~data:(Some s.env_length); *)
  let meta_env = RBMap.add_exn ~ord:ord_str meta_env name (Some s.env_length) in

  { s with meta_env = make_linear meta_env; env_length = s.env_length + 1 }

let drop_s s name =
  assert (Option.is_some (RBMap.find_exn ~ord:ord_str (read_linear s.meta_env) name));
  let meta_env = write_linear s.meta_env in
  (* Hashtbl.remove meta_env name; *)
  let meta_env = RBMap.erase ~ord:ord_str name meta_env in
  { s with meta_env = make_linear meta_env; env_length = s.env_length - 1 }

let pop_n s n =
  assert (s.env_length >= n);
  { s with meta_env = s.meta_env; env_length = s.env_length - n }

let pop_s s = pop_n s 1

(*todo: we actually need to dup a bunch. lets switch to a functional data structure. *)
let dup_s s = { s with meta_env = make_linear (read_linear s.meta_env); env_length = s.env_length }

type kont = { k : scope -> world code -> unit code; fv : string RBTree.t linear }

let dup_fv (fv : string RBTree.t linear) : string RBTree.t linear =
  make_linear (read_linear fv)

let empty_fv () : string RBTree.t linear = make_linear RBTree.empty

let drop (s : scope) (vars : string list) (w : world code) (k : kont) : unit code =
  let new_s, n =
    List.fold_left
      (fun (s, n) var ->
        match RBMap.find_exn ~ord:ord_str (read_linear s.meta_env) var with None -> (s, n) | Some _ -> (drop_s s var, n + 1))
      (s, 0) vars
  in
  seqs
    [
      (fun _ -> assert_env_length w (int s.env_length));
      (fun _ -> drop_n w (int s.env_length) (int n));
      (fun _ -> k.k new_s w);
    ]

let return (s : scope) (w : world code) : unit code =
  seq (assert_env_length w (int s.env_length)) (fun _ -> return_n w (int s.env_length) (pc_to_exp (int apply_cont)))

let add_fv (v : string) (fv : string RBTree.t linear) : string RBTree.t linear =
  let fv = write_linear fv in
  let (fv, _) = RBTree.add ~ord:ord_str fv v in
  make_linear fv

let remove_fv (v : string) (fv : string RBTree.t linear) : string RBTree.t linear =
  let fv = write_linear fv in
  let fv = RBTree.erase ~ord:ord_str v fv in
  make_linear fv

let rec fv_expr (e : expr) (fv : string RBTree.t linear) : string RBTree.t linear =
  match e with
  | Ctor _ | Int _ | GVar _ -> fv
  | App (f, xs) -> fv_exprs xs (fv_expr f fv)
  | Op (_, x, y) -> fv_expr y (fv_expr x fv)
  | Var name -> add_fv name fv
  | Match (value, cases) -> fv_expr value (fv_cases cases fv)
  | If (i, t, e) -> fv_expr i (fv_expr t (fv_expr e fv))
  | Let (BOne (l, v), r) -> fv_expr v (fv_pat l (fv_expr r fv))
  | _ -> failwith ("fv_expr: " ^ show_expr e)

and fv_exprs (es : expr list) (fv : string RBTree.t linear) : string RBTree.t linear =
  List.fold_left (fun fv e -> fv_expr e fv) fv es

and fv_pat (pat : pattern) (fv : string RBTree.t linear) : string RBTree.t linear =
  match pat with
  | PApp (_, None) -> fv
  | PApp (_, Some x) -> fv_pat x fv
  | PTup xs -> List.fold_left (fun fv x -> fv_pat x fv) fv xs
  | PVar name -> remove_fv name fv
  | _ -> failwith (show_pattern pat)

and fv_cases (MatchPattern c : cases) (fv : string RBTree.t linear) : string RBTree.t linear =
  List.fold_left (fun fv (pat, e) -> fv_pat pat (fv_expr e fv)) fv c

type keep_t = { mutable keep : bool; mutable source : string option }

let keep_only (s : scope) (fv : string RBTree.t linear) : int Dynarray.t * scope =
  let keep : keep_t Dynarray.t = Dynarray.init s.env_length (fun _ -> { keep = true; source = None }) in
  RBMap.iteri (read_linear s.meta_env) ~f:(fun key data ->
      match data with None -> () | Some i -> Dynarray.set keep i { keep = false; source = Some key });
  RBTree.iteri (read_linear fv) ~f:(fun v ->
      let i = Option.get (RBMap.find_exn ~ord:ord_str (read_linear s.meta_env) v) in
      (Dynarray.get keep i).keep <- true);
  let keep_idx : int Dynarray.t = Dynarray.create () in
  let (_, meta_env) = Dynarray.fold_left (fun (i, acc) k -> if k.keep then (
        Dynarray.add_last keep_idx i;
        match k.source with
        | None -> (i + 1, acc)
        | Some v -> (i + 1, RBMap.add_exn ~ord:ord_str acc v (Some (Dynarray.length keep_idx))))
      else (i + 1, acc)) (0, RBMap.empty) keep in
  let others = RBMap.map (fun _ _ -> None) (read_linear s.meta_env) in
  let meta_env = RBMap.append_trees meta_env others in
  (keep_idx, { s with meta_env = make_linear meta_env; env_length = Dynarray.length keep_idx })

let reading (s : scope) (f : scope -> world code -> unit code) (w : world code) : unit code =
  let make_code w = f { s with progressed = false } w in
  if s.progressed then goto w (add_code $ Some (lam "w" make_code)) else make_code w

let rec ant_pp_expr (ctx : ctx) (s : scope) (c : expr) (k : kont) : world code -> unit code =
  match c with
  | Var name ->
      let loc = Option.get (RBMap.find_exn ~ord:ord_str (read_linear s.meta_env) name) in
      fun w ->
        seqs
          [
            (fun _ -> assert_env_length w (int s.env_length));
            (fun _ -> push_env w (get_env w (int loc)));
            (fun _ -> k.k (push_s s) w);
          ]
  | Match (value, cases) ->
      ant_pp_expr ctx s value
        {
          k = (fun s -> reading s $ fun s w -> ant_pp_cases ctx (dup_s s) cases k w);
          fv = fv_cases cases (dup_fv k.fv);
        }
  | Ctor cname ->
      fun w ->
        seqs
          [
            (fun _ -> assert_env_length w (int s.env_length));
            (fun _ -> push_env w (from_constructor (int (Hashtbl.find_exn ctx.ctag cname))));
            (fun _ -> k.k (push_s s) w);
          ]
  | App (Ctor cname, [ x0 ]) ->
      ant_pp_expr ctx s x0
        {
          k =
            (fun s w ->
              seqs
                [
                  (fun _ -> assert_env_length w (int s.env_length));
                  (fun _ ->
                    let_in "x0" (pop_env w) (fun x0 ->
                        push_env w (memo_appends [ from_constructor (int (Hashtbl.find_exn ctx.ctag cname)); x0 ])));
                  (fun _ -> k.k (push_s (pop_s s)) w);
                ]);
          fv = k.fv;
        }
  | App (Ctor cname, [ x0; x1 ]) ->
      ant_pp_expr ctx s x0
        {
          k =
            (fun s w ->
              ant_pp_expr ctx s x1
                {
                  k =
                    (fun s w ->
                      seqs
                        [
                          (fun _ -> assert_env_length w (int s.env_length));
                          (fun _ ->
                            let_in "x1" (pop_env w) (fun x1 ->
                                let_in "x0" (pop_env w) (fun x0 ->
                                    push_env w
                                      (memo_appends
                                         [ from_constructor (int (Hashtbl.find_exn ctx.ctag cname)); x0; x1 ]))));
                          (fun _ -> k.k (push_s (pop_s (pop_s s))) w);
                        ]);
                  fv = k.fv;
                }
                w);
          fv = fv_expr x1 (dup_fv k.fv);
        }
  | App (GVar f, xs) ->
      let cont_name = "cont_" ^ string_of_int ctx.conts_count in
      let keep, keep_s = keep_only s k.fv in
      (* subtracting 1 to remove the arguments; adding 1 for the next continuation*)
      let keep_length = keep_s.env_length in
      add_cont ctx cont_name (keep_length + 1) (fun w tl ->
          seqs
            [
              (fun _ -> set_k w (get_next_cont tl));
              (fun _ -> restore_env w (int keep_length) tl);
              (fun _ -> k.k (push_s keep_s) w);
            ]);
      let xs_length = List.length xs in
      ant_pp_exprs ctx s xs
        {
          k =
            (fun s w ->
              seqs
                [
                  (fun _ -> assert_env_length w (int s.env_length));
                  (fun _ ->
                    let_in "keep"
                      (env_call w (list_literal_of int (Dynarray.to_list keep)) (int xs_length))
                      (fun keep ->
                        set_k w
                          (memo_appends
                             [ from_constructor (int (Hashtbl.find_exn ctx.ctag cont_name)); keep; world_kont w ])));
                  (fun _ -> goto w (Hashtbl.find_exn ctx.func_pc f));
                ]);
          fv = k.fv;
        }
  | Op ("+", x0, x1) ->
      ant_pp_expr ctx s x0
        {
          k =
            (fun s w ->
              ant_pp_expr ctx s x1
                {
                  k =
                    (fun s ->
                      reading s $ fun s w ->
                      seqs
                        [
                          (fun _ -> assert_env_length w (int s.env_length));
                          (fun _ ->
                            match_option
                              (resolve w (src_E (s.env_length - 2)))
                              (fun _ -> unit)
                              "x0"
                              (fun x0 ->
                                match_option
                                  (resolve w (src_E (s.env_length - 1)))
                                  (fun _ -> unit)
                                  "x1"
                                  (fun x1 ->
                                    seqs
                                      [
                                        (fun _ -> to_unit $ pop_env w);
                                        (fun _ -> to_unit $ pop_env w);
                                        (fun _ ->
                                          push_env w
                                            (memo_from_int (add (int_from_word (zro x0)) (int_from_word (zro x1)))));
                                        (fun _ -> k.k (push_s (pop_s (pop_s s))) w);
                                      ])));
                        ]);
                  fv = fv_expr x1 (dup_fv k.fv);
                }
                w);
          fv = k.fv;
        }
  | Int i ->
      fun w ->
        seqs
          [
            (fun _ -> assert_env_length w (int s.env_length));
            (fun _ -> push_env w (memo_from_int (int i)));
            (fun _ -> k.k (push_s s) w);
          ]
  | Let (BOne (PVar l, v), r) ->
      ant_pp_expr ctx s v
        {
          k =
            (fun s w ->
              ant_pp_expr ctx
                (extend_s (pop_s s) l)
                r
                { k = (fun s w -> drop s [ l ] w k); fv = add_fv l (dup_fv k.fv) }
                w);
          fv = fv_pat (PVar l) (fv_expr r (dup_fv k.fv));
        }
  | _ -> failwith ("ant_pp_expr: " ^ show_expr c)

and ant_pp_exprs (ctx : ctx) (s : scope) (cs : expr list) (k : kont) : world code -> unit code =
  match cs with
  | [] -> fun w -> k.k s w
  | c :: cs -> ant_pp_expr ctx s c { k = (fun s w -> ant_pp_exprs ctx s cs k w); fv = fv_exprs cs (dup_fv k.fv) }

and ant_pp_cases (ctx : ctx) (s : scope) (MatchPattern c : cases) (k : kont) : world code -> unit code =
 fun w ->
  seq
    (assert_env_length w (int s.env_length))
    (fun _ ->
      let_in "last"
        (src_E (s.env_length - 1))
        (fun last ->
          let m = resolve w last in
          match_option m
            (fun _ -> unit)
            "x"
            (fun x ->
              seq_b1
                (to_unit $ pop_env w)
                (fun _ ->
                  let s = pop_s s in
                  let t =
                    separate_map (break 1)
                      (fun (pat, expr) ->
                        let s = dup_s s in
                        (*todo: special casing for now, as pat design need changes. *)
                        match pat with
                        | PApp (cname, None) ->
                            string "| "
                            ^^ uncode (int (Hashtbl.find_exn ctx.ctag cname))
                            ^^ string " -> "
                            ^^ (uncode $ ant_pp_expr ctx s expr k w)
                        | PApp (cname, Some (PVar x0)) ->
                            string "| "
                            ^^ uncode (int (Hashtbl.find_exn ctx.ctag cname))
                            ^^ string " -> "
                            ^^
                            let x0_s = gensym "x0" |> string in
                            uncode
                              (seq
                                 (code
                                    (string "let [" ^^ x0_s ^^ string "] ="
                                    ^^ uncode (memo_splits (pair_value x))
                                    ^^ string " in "
                                    ^^ uncode (push_env w (code x0_s))))
                                 (fun _ ->
                                   ant_pp_expr ctx (extend_s s x0) expr
                                     { k = (fun s w -> drop s [ x0 ] w k); fv = k.fv }
                                     w))
                        | PApp (cname, Some (PTup [ PVar x0; PVar x1 ])) ->
                            string "| "
                            ^^ uncode (int (Hashtbl.find_exn ctx.ctag cname))
                            ^^ string " -> "
                            ^^
                            let x0_s = gensym "x0" |> string in
                            let x1_s = gensym "x1" |> string in
                            uncode
                              (seq
                                 (code
                                    (string "let [" ^^ x0_s ^^ string "; " ^^ x1_s ^^ string "] ="
                                    ^^ uncode (memo_splits (pair_value x))
                                    ^^ string " in "
                                    ^^ uncode (push_env w (code x0_s))))
                                 (fun _ ->
                                   seq
                                     (push_env w (code x1_s))
                                     (fun _ ->
                                       ant_pp_expr ctx
                                         (extend_s (extend_s s x0) x1)
                                         expr
                                         { k = (fun s w -> drop s [ x1; x0 ] w k); fv = k.fv }
                                         w)))
                        | PApp (cname, Some (PTup [ PVar x0; PVar x1; PVar x2 ])) ->
                            string "| "
                            ^^ uncode (int (Hashtbl.find_exn ctx.ctag cname))
                            ^^ string " -> "
                            ^^
                            let x0_s = gensym "x0" |> string in
                            let x1_s = gensym "x1" |> string in
                            let x2_s = gensym "x2" |> string in
                            uncode
                              (seq
                                 (code
                                    (string "let [" ^^ x0_s ^^ string "; " ^^ x1_s ^^ string "; " ^^ x2_s
                                   ^^ string "] ="
                                    ^^ uncode (memo_splits (pair_value x))
                                    ^^ string " in "
                                    ^^ uncode (push_env w (code x0_s))))
                                 (fun _ ->
                                   seqs
                                     [
                                       (fun _ -> push_env w (code x1_s));
                                       (fun _ -> push_env w (code x2_s));
                                       (fun _ ->
                                         ant_pp_expr ctx
                                           (extend_s (extend_s (extend_s s x0) x1) x2)
                                           expr
                                           { k = (fun s w -> drop s [ x2; x1; x0 ] w k); fv = k.fv }
                                           w);
                                     ]))
                        | _ -> failwith (show_pattern pat))
                      c
                  in
                  code
                    (string " (match "
                    ^^ uncode (word_get_value (zro x))
                    ^^ string " with "
                    ^^ t
                    ^^ string ")")))))

let ant_pp_stmt (ctx : ctx) (s : stmt) : document =
  match s with
  | Type (TBOne (name, Enum { params = _; ctors })) -> ant_pp_adt ctx name ctors
  | Type (TBRec _) -> failwith "Not implemented (TODO)"
  | Term (x, Lam (ps, term)) ->
      let s =
        List.fold_left
          (fun s p -> match p with PVar n -> extend_s s n | _ -> failwith (show_pattern p))
          (new_scope ()) ps
      in
      let arg_num = s.env_length in
      let name = match x with Some (PVar x) -> x | _ -> failwith "bad match" in
      let cont_done_tag = int (Hashtbl.find_exn ctx.ctag "cont_done") in
      add_code_k (fun entry_code ->
          Hashtbl.add_exn ctx.func_pc ~key:name ~data:entry_code;
          ( lam "w" (fun w -> ant_pp_expr ctx s term { k = (fun s w -> return s w); fv = empty_fv () } w),
            string "let rec" ^^ space ^^ string name ^^ space
            ^^ separate space (List.init arg_num (fun i -> string ("(x" ^ string_of_int i ^ " : Value.seq)")))
            ^^ string ": Value.seq " ^^ string "=" ^^ space ^^ group @@ string "exec_cek "
            ^^ string ("(pc_to_exp " ^ string_of_int entry_code ^ ")")
            ^^ string "(Dynarray.of_list" ^^ string "["
            ^^ separate (string ";") (List.init arg_num (fun i -> string ("(x" ^ string_of_int i ^ ")")))
            ^^ string "]" ^^ string ")" ^^ string "(" ^^ uncode (from_constructor cont_done_tag) ^^ string ")"
            ^^ string " memo" ))
  | Fun (_name, _args, _body) -> failwith "Not implemented (TODO)"
  | _ -> failwith (show_stmt s)

let generate_apply_cont ctx =
  set_code apply_cont
    (lam "w" (fun w ->
         (* We have to be careful: ctx.conts will grow as we apply the lambdas, 
          *   so we cannot do a single map over the whole array, 
          *   instead we have to take elements out on by one.
          *)
         let cont_codes = Dynarray.create () in
         let rec loop i =
           if i == Dynarray.length ctx.conts then cont_codes
           else
             let name, action = Dynarray.get ctx.conts i in
             let code =
               string "| "
               ^^ uncode (int (Hashtbl.find_exn ctx.ctag name))
               ^^ string " -> "
               ^^ uncode (action w (code $ string "tl"))
             in
             Dynarray.add_last cont_codes code;
             loop (i + 1)
         in
         seq
           (assert_env_length w (int 1))
           (fun _ ->
             code
             $ string "match resolve " ^^ uncode w
               ^^ string " K with | None -> () | Some (hd, tl) -> match "
               ^^ uncode (word_get_value (code $ string "hd"))
               ^^ string " with "
               ^^ separate (break 1) (Dynarray.to_list (loop 0)))))

let generate_apply_cont_ ctx =
  set_code apply_cont
    (lam "w" (fun w ->
         seq
           (assert_env_length w (int 1))
           (fun _ ->
             code
             $ string "match resolve " ^^ uncode w
               ^^ string " K with | None -> () | Some (hd, tl) -> match "
               ^^ uncode (word_get_value (code $ string "hd"))
               ^^ string " with "
               ^^ separate_map (break 1)
                    (fun (name, action) ->
                      string "| "
                      ^^ uncode (int (Hashtbl.find_exn ctx.ctag name))
                      ^^ string " -> "
                      ^^ uncode (action w (code $ string "tl")))
                    (Dynarray.to_list ctx.conts))))

let pp_cek_ant x =
  let ctx = new_ctx () in
  let generated_stmt = separate_map (break 1) (ant_pp_stmt ctx) x in
  generate_apply_cont ctx;
  string "open Ant" ^^ break 1 ^^ string "open Word" ^^ break 1 ^^ string "open Memo" ^^ break 1 ^^ string "open Value"
  ^^ break 1 ^^ string "let memo = Array.init "
  ^^ uncode (int (Dynarray.length codes))
  ^^ string "(fun _ -> ref State.BlackHole)" ^^ break 1 ^^ generated_stmt ^^ break 1
  ^^ separate (break 1)
       (List.init (Dynarray.length codes) (fun i ->
            string "let () = add_exp "
            ^^ uncode (Option.get (Dynarray.get codes i))
            ^^ string " "
            ^^ uncode (int i)))
  ^^ break 1
  ^^ separate (break 1)
       (List.init (Dynarray.length ctx.constructor_degree) (fun i ->
            string "let () = Value.set_constructor_degree "
            ^^ uncode (int i)
            ^^ string " ("
            ^^ uncode (int (Dynarray.get ctx.constructor_degree i))
            ^^ string ")"))
