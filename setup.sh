#!/bin/sh

opam switch create ant 5.2.1
eval $(opam env --switch=ant)
opam install \
    core dune menhir \
    ppx_deriving ppx_sexp_conv \
    yojson core_unix batteries \
    pprint cmdliner core_bench \
    ocaml-lsp-server ocamlformat \
    -y
