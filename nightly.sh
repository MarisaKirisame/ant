#!/usr/bin/env bash
set -euo pipefail

SWITCH=${OPAM_SWITCH:-ant}
PACKAGES="core dune menhir ppx_deriving ppx_sexp_conv yojson core_unix batteries pprint cmdliner core_bench"

ensure_switch() {
  opam switch create "$SWITCH" --empty || true
  opam switch "$SWITCH"
  opam switch set-invariant --update-invariant "ocaml>=5.2" -y
}

activate_switch() {
  # shellcheck disable=SC2046
  eval $(opam env --switch="$SWITCH")
}

install_dependencies() {
  ensure_switch
  activate_switch
  opam update
  opam upgrade --fixup -y
  opam install $PACKAGES -y
}

build_project() {
  ensure_switch
  activate_switch
  dune build
}

run_project() {
  ensure_switch
  activate_switch
  export OCAMLRUNPARAM=b
  dune exec ant -- examples/Test.ant generated/TestSeq.ml --print-ant
  dune exec ant -- examples/Test.ant generated/TestCEK.ml --cek
  dune fmt &> /dev/null || true
  dune exec GeneratedMain
}

stage=${1:-all}

case "$stage" in
  dependency)
    install_dependencies
    ;;
  build)
    build_project
    ;;
  run)
    run_project
    ;;
  all)
    install_dependencies
    build_project
    run_project
    ;;
  *)
    echo "Unknown stage: $stage" >&2
    echo "Usage: $0 [dependency|build|run|all]" >&2
    exit 1
    ;;
esac
