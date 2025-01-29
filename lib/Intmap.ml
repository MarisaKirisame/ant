type intmap

external create : int -> intmap = "intmap_create_stub"

external clear : intmap -> unit = "intmap_clear_stub" [@@noalloc]

external reset : intmap -> unit = "intmap_reset_stub" [@@noalloc]

external add: intmap -> int -> int -> unit = "intmap_add_stub" [@@noalloc]

external remove : intmap -> int -> unit = "intmap_remove_stub" [@@noalloc]

external find: intmap -> int -> int = "intmap_find_stub"

external find_opt : intmap -> int -> int option = "intmap_find_opt_stub"

external mem : intmap -> int -> bool = "intmap_mem_stub" [@@noalloc]

external length : intmap -> int = "intmap_length_stub"


