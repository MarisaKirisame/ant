type eviction_policy = { retain_ratio : float; kll_k : int }
type eviction_state = { mutable max_tree_size_seen : int }

val make_eviction_policy : retain_ratio:float -> kll_k:int -> eviction_policy
val init_eviction_state : unit -> eviction_state
val update_max_tree_size_seen : eviction_state -> current_size:int -> unit
val target_size_from_state : policy:eviction_policy -> state:eviction_state -> int
val score_step : State.step -> int
val trie_size : State.trie -> int
val memo_size : State.memo -> int
val stream_scores_from_trie : kll:Kll.t -> State.trie -> unit
val stream_scores_from_memo : kll:Kll.t -> State.memo -> unit
val eviction_fraction_for_target : target_size:int -> State.memo -> float
val prune_memo_to_target : kll_k:int -> evict_fraction:float -> State.memo -> State.memo
val batch_evict_memo : policy:eviction_policy -> state:eviction_state -> State.memo -> State.memo
