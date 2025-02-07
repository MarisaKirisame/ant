let driver intmap hash list =
  if intmap then Intmap_bench.bench_intmap ();
  if hash then MonoidHash_bench.bench_monoid_hash ();
  if list then List_bench.bench_list ()

let _ =
  driver
    (Option.is_some (Sys.getenv_opt "INTMAP"))
    (Option.is_some (Sys.getenv_opt "MONOIDHASH"))
    (Option.is_some (Sys.getenv_opt "LIST"))
