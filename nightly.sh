opam switch create megatron --empty || true
opam switch megatron
eval $(opam env)
opam update
opam upgrade -y
opam install core dune menhir ppx_deriving ppx_sexp_conv yojson core_unix batteries pprint cmdliner core_bench -y
dune build