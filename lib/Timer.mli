type t

external create : unit -> t = "ns_timer_create"
external record : t -> unit = "ns_timer_record"
external diff_nanoseconds : t -> t -> int64 = "ns_timer_diff"
val diff_nanoseconds_positive : t -> t -> int64
