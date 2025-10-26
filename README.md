# Ant

Ant is an experimental incremental evaluator that memoises *prefixes* of CEK machine executions.  
Instead of caching whole function calls, Ant records reusable fragments of the control–environment–continuation state so that later runs can skip long stretches of evaluation when the input shares a prefix.

## How It Works
- **CEK machine core.**  Programs are compiled to a control table; the evaluator runs a CEK machine `(C, E, K)` and records memo entries keyed by control location plus environment/continuation slices.
- **Finger-tree values.**  Environments and continuations are stored as finger trees annotated with monoid hashes and degree measures.  This lets Ant split/compare prefixes without rescanning the full structure (see `internal.md` for details).
- **Prefix memoisation.**  When the current state matches a memoised fragment `F[XS] -> G[XS]`, Ant substitutes the live bindings and jumps directly to `G`, avoiding redundant work on the shared prefix.

For a deeper design write-up, start with `internal.md`.

## Getting Started
```bash
make dependency   # install opam switch + packages (idempotent)
make build        # dune build
make run          # builds, formats, runs examples/Test.ant through the generated CEK
```

The `nightly.sh` script underpins the Make targets; expect it to print a harmless warning if the `ant` opam switch already exists.

## Repository Layout
- `lib/` – OCaml sources (CEK evaluator, memo infrastructure, value representations).
- `generated/` – Auto-generated OCaml modules (rebuilt by `make`).
- `examples/` – Sample `.ant` programs; `examples/Test.ant` is the default demo.
- `bench/` – Benchmarks.
- `internal.md` – In-depth notes on the memoisation approach.
- `AGENTS.md` – How the Codex agent works in this repo.

## Contributing
Pull requests are welcome.  Please run `make build` before submitting and highlight any remaining warnings or TODOs in the description.
