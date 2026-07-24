(lang dune 3.16)
(using menhir 3.0)
(using ctypes 0.3)

(name ant)

(generate_opam_files true)

(maintainers "Marisa Kirisame <marisa@cs.utah.edu>")

(license Apache-2.0)

(package
 (name ant)
 (synopsis "Incremental live programming via shortcut memoization")
 (description
  "The Chordata compiler, runtime, benchmarks, and artifact evaluation")
 (depends
  (ocaml (= 5.4.1))
  (ocaml-base-compiler (= 5.4.1))
  dune
  menhir
  pprint
  cmdliner
  batteries
  core
  core_unix
  core_bench
  sexplib
  yojson
  ppx_deriving
  ppxlib))
