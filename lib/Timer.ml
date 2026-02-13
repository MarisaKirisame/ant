type t

external create : unit -> t = "ns_timer_create"
external record : t -> unit = "ns_timer_record"

(* t_2 - t_1 *)
external diff_nanoseconds : t -> t -> int64 = "ns_timer_diff"
