module type S = sig
  type ('k, 'v) t

  val name : string
  val create : ?size:int -> unit -> ('k, 'v) t
  val length : ('k, 'v) t -> int
  val is_empty : ('k, 'v) t -> bool
  val find : ('k, 'v) t -> 'k -> 'v option
  val find_opt : ('k, 'v) t -> 'k -> 'v option
  val find_exn : ('k, 'v) t -> 'k -> 'v
  val mem : ('k, 'v) t -> 'k -> bool
  val set : ('k, 'v) t -> key:'k -> data:'v -> unit
  val add : ('k, 'v) t -> key:'k -> data:'v -> [ `Ok | `Duplicate ]
  val add_exn : ('k, 'v) t -> key:'k -> data:'v -> unit
  val remove : ('k, 'v) t -> 'k -> unit
  val iter : ('k, 'v) t -> f:('v -> unit) -> unit
  val to_alist : ('k, 'v) t -> ('k * 'v) list
  val of_seq : ('k * 'v) Seq.t -> ('k, 'v) t
end

module Small (Fallback : S) : S = struct
  type ('k, 'v) small = { items : ('k * 'v) list; size : int }
  type ('k, 'v) repr = Small of ('k, 'v) small | Fallback of ('k, 'v) Fallback.t
  type ('k, 'v) t = { mutable repr : ('k, 'v) repr }

  (* Keep tiny tables in a flat list, then promote once they stop being tiny. *)
  let small_limit = 8
  let name = "small+" ^ Fallback.name
  let length t = match t.repr with Small small -> small.size | Fallback tbl -> Fallback.length tbl
  let is_empty t = match t.repr with Small small -> small.size = 0 | Fallback tbl -> Fallback.is_empty tbl

  let to_fallback (small : ('k, 'v) small) =
    let tbl = Fallback.create ~size:small.size () in
    List.iter (fun (key, value) -> Fallback.set tbl ~key ~data:value) small.items;
    tbl

  let remove_assoc_with_flag key items =
    let rec loop removed acc = function
      | [] -> (List.rev acc, removed)
      | ((k, _) as pair) :: rest -> if k = key then loop true acc rest else loop removed (pair :: acc) rest
    in
    loop false [] items

  let promote_if_needed t =
    match t.repr with Small small when small.size > small_limit -> t.repr <- Fallback (to_fallback small) | _ -> ()

  let create ?size () =
    match size with
    | Some size when size > small_limit -> { repr = Fallback (Fallback.create ~size ()) }
    | _ -> { repr = Small { items = []; size = 0 } }

  let find t key =
    match t.repr with Small small -> List.assoc_opt key small.items | Fallback tbl -> Fallback.find tbl key

  let find_opt = find

  let find_exn t key =
    match t.repr with Small small -> List.assoc key small.items | Fallback tbl -> Fallback.find_exn tbl key

  let mem t key =
    match t.repr with Small small -> List.mem_assoc key small.items | Fallback tbl -> Fallback.mem tbl key

  let set t ~key ~data =
    match t.repr with
    | Small small ->
        let filtered, removed = remove_assoc_with_flag key small.items in
        let size = if removed then small.size else small.size + 1 in
        t.repr <- Small { items = (key, data) :: filtered; size };
        promote_if_needed t
    | Fallback tbl -> Fallback.set tbl ~key ~data

  let add t ~key ~data =
    match t.repr with
    | Small small ->
        if List.mem_assoc key small.items then `Duplicate
        else (
          t.repr <- Small { items = (key, data) :: small.items; size = small.size + 1 };
          promote_if_needed t;
          `Ok)
    | Fallback tbl -> Fallback.add tbl ~key ~data

  let add_exn t ~key ~data =
    match add t ~key ~data with `Ok -> () | `Duplicate -> failwith "Hashtbl.add_exn: duplicate key"

  let remove t key =
    match t.repr with
    | Small small ->
        let filtered, removed = remove_assoc_with_flag key small.items in
        let size = if removed then small.size - 1 else small.size in
        t.repr <- Small { items = filtered; size }
    | Fallback tbl -> Fallback.remove tbl key

  let iter t ~f =
    match t.repr with
    | Small small -> List.iter (fun (_, value) -> f value) small.items
    | Fallback tbl -> Fallback.iter tbl ~f

  let to_alist t = match t.repr with Small small -> small.items | Fallback tbl -> Fallback.to_alist tbl

  let of_seq seq =
    let t = create () in
    Seq.iter (fun (key, data) -> set t ~key ~data) seq;
    t
end

module Stdlib : S = struct
  type ('k, 'v) t = ('k, 'v) Stdlib.Hashtbl.t

  let name = "stdlib"

  let create ?size () =
    let size = Option.value size ~default:16 in
    Stdlib.Hashtbl.create size

  let length = Stdlib.Hashtbl.length
  let is_empty t = Stdlib.Hashtbl.length t = 0
  let find t key = Stdlib.Hashtbl.find_opt t key
  let find_opt = find
  let find_exn t key = Stdlib.Hashtbl.find t key
  let mem = Stdlib.Hashtbl.mem
  let set t ~key ~data = Stdlib.Hashtbl.replace t key data

  let add t ~key ~data =
    if mem t key then `Duplicate
    else (
      Stdlib.Hashtbl.add t key data;
      `Ok)

  let add_exn t ~key ~data =
    match add t ~key ~data with `Ok -> () | `Duplicate -> failwith "Hashtbl.add_exn: duplicate key"

  let remove = Stdlib.Hashtbl.remove
  let iter t ~f = Stdlib.Hashtbl.iter (fun _ v -> f v) t
  let to_alist t = Stdlib.Hashtbl.to_seq t |> List.of_seq

  let of_seq seq =
    let t = create () in
    Seq.iter (fun (k, v) -> set t ~key:k ~data:v) seq;
    t
end

module Core : S = struct
  type ('k, 'v) t = ('k, 'v) Core.Hashtbl.Poly.t

  let name = "core"
  let create ?size () = Core.Hashtbl.Poly.create ?size ()
  let length = Core.Hashtbl.length
  let is_empty = Core.Hashtbl.is_empty
  let find = Core.Hashtbl.find
  let find_opt = Core.Hashtbl.find
  let find_exn = Core.Hashtbl.find_exn
  let mem = Core.Hashtbl.mem
  let set = Core.Hashtbl.set
  let add = Core.Hashtbl.add
  let add_exn = Core.Hashtbl.add_exn
  let remove = Core.Hashtbl.remove
  let iter t ~f = Core.Hashtbl.iter t ~f
  let to_alist = Core.Hashtbl.to_alist

  let of_seq seq =
    let t = create () in
    Seq.iter (fun (k, v) -> set t ~key:k ~data:v) seq;
    t
end

module Small_stdlib = Small (Stdlib)
module Small_core = Small (Core)

(* One-line switch between implementations. *)
module Impl = Small_stdlib
include Impl
