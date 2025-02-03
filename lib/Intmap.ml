type intmap

external create : int -> intmap = "intmap_create_stub"
external clear : intmap -> unit = "intmap_clear_stub" [@@noalloc]
external reset : intmap -> unit = "intmap_reset_stub" [@@noalloc]

external add : intmap -> (int[@untagged]) -> (int[@untagged]) -> unit = "intmap_add_stub" "intmap_add_untagged_stub"
[@@noalloc]

external remove : intmap -> (int[@untagged]) -> unit = "intmap_remove_stub" "intmap_remove_untagged_stub" [@@noalloc]
external find : intmap -> (int[@untagged]) -> (int[@untagged]) = "intmap_find_stub" "intmap_find_untagged_stub"
external find_opt : intmap -> (int[@untagged]) -> int option = "intmap_find_opt_stub" "intmap_find_opt_untagged_stub"

external mem : intmap -> (int[@untagged]) -> (bool[@untagged]) = "intmap_mem_stub" "intmap_mem_untagged_stub"
[@@noalloc]

external iter : (int -> int -> unit) -> intmap -> unit = "intmap_iter_stub" [@@noalloc]
external fold : (int -> int -> 'a -> 'a) -> intmap -> 'a -> 'a = "intmap_fold_stub" [@@noalloc]
external length : intmap -> int = "intmap_length_stub"
