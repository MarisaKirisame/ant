(* dummy is used to allow info record extended with `with` syntax *)

type info = { ty : Type.ty option; dummy : unit }

let empty_info = { ty = None; dummy = () }
