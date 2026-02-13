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

module Small : S = struct
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

(* One-line switch between implementations. *)
module Impl = Core
include Impl
