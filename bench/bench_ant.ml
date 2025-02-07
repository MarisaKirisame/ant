let driver intmap hash =
  if intmap then Intmap_bench.bench_intmap ();
  if hash then MonoidHash_bench.bench_monoid_hash ()

let _ = driver (Option.is_some (Sys.getenv_opt "INTMAP")) (Option.is_some (Sys.getenv_opt "MONOIDHASH"))
