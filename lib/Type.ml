open! Core
open PPrint

module Level = struct
  module Make () : sig
    type t

    val enter : unit -> unit
    val leave : unit -> unit
    val current_level : unit -> t
    val generic_level : t
    val marker_level : t
    val reset : unit -> unit
    val ( < ) : t -> t -> bool
    val ( = ) : t -> t -> bool
    val ( <= ) : t -> t -> bool
    val max : t -> t -> t
    val min : t -> t -> t
    val to_string : t -> string
  end = struct
    type t = int

    let current = ref 1
    let enter () = incr current
    let leave () = decr current
    let current_level () = !current
    let generic_level = Int.max_value
    let marker_level = Int.min_value
    let reset () = current := 1
    let ( < ) x y = Int.(x < y)
    let ( = ) x y = Int.(x = y)
    let ( <= ) x y = Int.(x <= y)
    let max x y = Int.max x y
    let min x y = Int.min x y
    let to_string x = string_of_int x
  end
end

module TyLevel = Level.Make ()
module TyFresh = Fresh.Make ()
module StrMap = Map.Make (String)

type pty = Unit | Int | Float | Bool | Str [@@deriving equal]

type ty =
  | TPrim of pty
  | TTup of ty list * levels
  | TArr of ty list * levels
  | TArrow of ty list * ty * levels
  | TVar of tv ref
  | TNamed of string

and tv = Link of ty | Unbound of string * TyLevel.t (* This string is not necessary, but it's helpful for debugging *)
and levels = { mutable level_old : TyLevel.t; mutable level_new : TyLevel.t }

let rec pp_ty = function
  | TPrim Unit -> string "unit"
  | TPrim Int -> string "int"
  | TPrim Float -> string "float"
  | TPrim Bool -> string "bool"
  | TPrim Str -> string "str"
  | (TTup (_, ls) | TArr (_, ls) | TArrow (_, _, ls)) when TyLevel.(ls.level_new = marker_level) ->
      string "<back-reference>"
  | TTup (ts, ls) ->
      let level = ls.level_new in
      ls.level_new <- TyLevel.marker_level;
      let doc =
        string "("
        ^^ separate_map (string ", ") pp_ty ts
        ^^ string ")" ^^ colon
        ^^ string (TyLevel.to_string level)
        ^^ tilde
        ^^ string (TyLevel.to_string ls.level_old)
      in
      ls.level_new <- level;
      doc
  | TArr (ts, ls) ->
      let level = ls.level_new in
      ls.level_new <- TyLevel.marker_level;
      let doc =
        string "["
        ^^ separate_map (string ", ") pp_ty ts
        ^^ string "]" ^^ colon
        ^^ string (TyLevel.to_string level)
        ^^ tilde
        ^^ string (TyLevel.to_string ls.level_old)
      in
      ls.level_new <- level;
      doc
  | TArrow (ts, ty, ls) ->
      let level = ls.level_new in
      ls.level_new <- TyLevel.marker_level;
      let doc =
        string "("
        ^^ separate_map (string ", ") pp_ty ts
        ^^ string " -> " ^^ pp_ty ty ^^ string ")" ^^ colon
        ^^ string (TyLevel.to_string level)
        ^^ tilde
        ^^ string (TyLevel.to_string ls.level_old)
      in
      ls.level_new <- level;
      doc
  | TVar { contents = Unbound (name, l) } -> string "'" ^^ string name ^^ colon ^^ string (TyLevel.to_string l)
  | TVar { contents = Link ty } -> pp_ty ty
  | TNamed name -> string name

let rec repr = function
  | TVar ({ contents = Link t } as tvr) ->
      let t = repr t in
      tvr := Link t;
      t
  | t -> t

let get_level = function
  | TVar { contents = Unbound (_, l) } -> l
  | TArrow (_, _, levels) -> levels.level_new
  | _ -> failwith "get_level: type without level assigned"

let new_tvar () = TVar (ref (Unbound (TyFresh.next_sym (), TyLevel.current_level ())))
let new_generic_tvar () = TVar (ref (Unbound (TyFresh.next_sym (), TyLevel.generic_level)))

let new_arrow t1 t2 =
  let level = TyLevel.current_level () in
  TArrow (t1, t2, { level_new = level; level_old = level })

let new_tup ts =
  let level = TyLevel.current_level () in
  TTup (ts, { level_new = level; level_old = level })

let new_arr ts =
  let level = TyLevel.current_level () in
  TArr (ts, { level_new = level; level_old = level })
