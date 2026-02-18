open Word

(* Legacy stubs for the tree backend. Finger-tree measures are no longer used. *)

type words = Word.t
type measure = unit

let reset () = ()
let set_constructor_degree (_ctag : int) (_degree : int) : unit = ()
let string_of_words (w : words) : string = Word.to_string w
