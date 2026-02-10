type t

external create : unit -> t = "timer_create"
external record : t -> unit = "timer_record"
external diff_nanoseconds : t -> t -> int64 = "timer_diff_nanoseconds"
