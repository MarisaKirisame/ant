type t

val create : ?k:int -> ?seed:int -> unit -> t
val add : t -> int -> unit
val add_many : t -> int array -> unit
val quantile : t -> q:float -> int option
