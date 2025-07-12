opam switch create ant --empty || true
opam switch ant
opam switch set-invariant --update-invariant "ocaml>=5.2" -y
eval $(opam env)
opam update
opam upgrade --fixup -y
opam install core dune menhir ppx_deriving ppx_sexp_conv yojson core_unix batteries pprint cmdliner core_bench -y
dune build
export OCAMLRUNPARAM=b; (dune exec ant -- examples/Test.ant --cek > generated/TestCEK.ml) && (dune fmt || true) && dune exec GeneratedMain
