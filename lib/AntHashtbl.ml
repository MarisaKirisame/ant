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

module Small_list : S = struct
  type ('k, 'v) t = { mutable data : ('k * 'v) list }

  let name = "small"
  let create ?size:_ () = { data = [] }
  let length t = List.length t.data
  let is_empty t = t.data = []
  let find t key = List.assoc_opt key t.data
  let find_opt = find
  let find_exn t key = List.assoc key t.data
  let mem t key = List.mem_assoc key t.data

  let set t ~key ~data =
    let filtered = List.filter (fun (k, _) -> k <> key) t.data in
    t.data <- (key, data) :: filtered

  let add t ~key ~data =
    if mem t key then `Duplicate
    else (
      t.data <- (key, data) :: t.data;
      `Ok)

  let add_exn t ~key ~data =
    match add t ~key ~data with `Ok -> () | `Duplicate -> failwith "Hashtbl.add_exn: duplicate key"

  let remove t key = t.data <- List.filter (fun (k, _) -> k <> key) t.data
  let iter t ~f = List.iter (fun (_, v) -> f v) t.data
  let to_alist t = t.data

  let of_seq seq =
    let t = create () in
    Seq.iter (fun (k, v) -> set t ~key:k ~data:v) seq;
    t
end

module Small (Fallback : S) : S = struct
  type ('k, 'v) repr = Small of ('k * 'v) list | Fallback of ('k, 'v) Fallback.t
  type ('k, 'v) t = { mutable repr : ('k, 'v) repr }

  (* Keep tiny tables in a flat list, then promote once they stop being tiny. *)
  let small_limit = 16
  let name = "small+" ^ Fallback.name
  let length t = match t.repr with Small data -> List.length data | Fallback tbl -> Fallback.length tbl
  let is_empty t = match t.repr with Small data -> data = [] | Fallback tbl -> Fallback.is_empty tbl

  let to_fallback data =
    let tbl = Fallback.create ~size:(List.length data) () in
    List.iter (fun (key, value) -> Fallback.set tbl ~key ~data:value) data;
    tbl

  let promote_if_needed t =
    match t.repr with
    | Small data when List.length data > small_limit -> t.repr <- Fallback (to_fallback data)
    | _ -> ()

  let demote_if_needed t =
    match t.repr with
    | Fallback tbl when Fallback.length tbl <= small_limit -> t.repr <- Small (Fallback.to_alist tbl)
    | _ -> ()

  let create ?size () =
    match size with
    | Some size when size > small_limit -> { repr = Fallback (Fallback.create ~size ()) }
    | _ -> { repr = Small [] }

  let find t key = match t.repr with Small data -> List.assoc_opt key data | Fallback tbl -> Fallback.find tbl key
  let find_opt = find
  let find_exn t key = match t.repr with Small data -> List.assoc key data | Fallback tbl -> Fallback.find_exn tbl key
  let mem t key = match t.repr with Small data -> List.mem_assoc key data | Fallback tbl -> Fallback.mem tbl key

  let set t ~key ~data =
    match t.repr with
    | Small items ->
        t.repr <- Small ((key, data) :: List.filter (fun (k, _) -> k <> key) items);
        promote_if_needed t
    | Fallback tbl -> Fallback.set tbl ~key ~data

  let add t ~key ~data =
    match t.repr with
    | Small items ->
        if List.mem_assoc key items then `Duplicate
        else (
          t.repr <- Small ((key, data) :: items);
          promote_if_needed t;
          `Ok)
    | Fallback tbl -> Fallback.add tbl ~key ~data

  let add_exn t ~key ~data =
    match add t ~key ~data with `Ok -> () | `Duplicate -> failwith "Hashtbl.add_exn: duplicate key"

  let remove t key =
    match t.repr with
    | Small items -> t.repr <- Small (List.filter (fun (k, _) -> k <> key) items)
    | Fallback tbl ->
        Fallback.remove tbl key;
        demote_if_needed t

  let iter t ~f =
    match t.repr with
    | Small items -> List.iter (fun (_, value) -> f value) items
    | Fallback tbl -> Fallback.iter tbl ~f

  let to_alist t = match t.repr with Small items -> items | Fallback tbl -> Fallback.to_alist tbl

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
