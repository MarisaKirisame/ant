type t

external create : unit -> t = "timer_create"
external record : t -> unit = "timer_record"

(* t_2 - t_1 *)
external diff_nanoseconds : t -> t -> int64 = "timer_diff_nanoseconds"
