# Ant

Ant is an experimental incremental evaluator that memoises *prefixes* of CEK machine executions.  
Instead of caching whole function calls, Ant records reusable fragments of the control–environment–continuation state so that later runs can skip long stretches of evaluation when the input shares a prefix.

## How It Works
- **CEK machine core.**  Programs are compiled to a control table; the evaluator runs a CEK machine `(C, E, K)` and records memo entries keyed by control location plus environment/continuation slices.
- **Finger-tree values.**  Environments and continuations are stored as finger trees annotated with monoid hashes and degree measures.  This lets Ant split/compare prefixes without rescanning the full structure (see `internal.md` for details).
- **Prefix memoisation.**  When the current state matches a memoised fragment `F[XS] -> G[XS]`, Ant substitutes the live bindings and jumps directly to `G`, avoiding redundant work on the shared prefix.

For a deeper design write-up, start with `internal.md`.

## Architecture at a Glance
- **Front-end.**  `Lexer.mll`, `Parser.mly`, and `Syntax.ml` define the surface language, with `Resolve.ml` promoting known functions to globals before type checking.
- **Type inference.**  `Tyck.ml` hosts a constraint-driven Hindley–Milner engine that annotates the AST and exposes debug printers.
- **Normalisation.**  `Transform.ml` implements CPS and defunctionalisation passes; `Pat.ml` lowers pattern matrices to decision trees.
- **Code generation.**  `CompileSeq.ml` emits a pure sequence interpreter, whereas `CompileMemo.ml` builds the memoising CEK VM via the combinators in `Code.ml`/`Ir.ml`.
- **Runtime core.**  Prefix memo tables and the finger-tree runtime live in `Memo.ml`, `Seq.ml`, `Value.ml`, `State.ml`, and `Word.ml`.

See the refreshed `internal.md` for details on the data structures these modules share.

## Getting Started
```bash
make dependency   # install opam switch + packages (idempotent)
make build        # dune build
make run          # builds, formats, runs examples/Test.ant through the generated CEK
```

The `nightly.py` script underpins the Make targets; expect it to print a harmless warning if the `ant` opam switch already exists.

## Repository Layout
- `lib/` – OCaml sources (CEK evaluator, memo infrastructure, value representations).
- `generated/` – Auto-generated OCaml modules (rebuilt by `make`).
- `examples/` – Sample `.ant` programs; `examples/Test.ant` is the default demo.
- `bench/` – Benchmarks.
- `internal.md` – In-depth notes on the memoisation approach.
- `AGENTS.md` – How the Codex agent works in this repo.

## Command-Line Interface
Run `dune exec ant -- INPUT OUTPUT [flags]` to compile a program.

- `-p`, `--print-ast` – pretty-print the resolved surface syntax.
- `--pat`, `--compile-pat` – dump the compiled pattern matrices from `Pat`.
- `-a`, `--print-ant` – emit the sequential interpreter from `CompileSeq`.
- `--cek`, `--print-cek-ant` – emit the memoising CEK VM from `CompileMemo`.
- `-t`, `--tyck` – print inferred types from `Tyck`.
- `-c`, `--print-cps`, `-d`, `--print-defunc`, `-D`, `--print-cps-defunc` – expose CPS/defunctionalised intermediates from `Transform`.

The driver wiring for these switches lives in `bin/main.ml`.

## Development Workflow
- `make dependency` – create/update the local `ant` opam switch and install the packages listed in `nightly.py`.
- `make build` – build through the managed switch (`dune build`).
- `make run` – regenerate `generated/*.ml`, format, and execute `GeneratedMain`.
- `dune runtest` – execute unit tests in `test/test_ant.ml` (hash monoid and intmap sanity checks).
- `dune exec bench/<target>.exe` – run benchmarks in `bench/`.

Generated modules under `generated/` are overwritten by `make run`; avoid editing them manually.

## Contributing
Pull requests are welcome.  Please run `make build` before submitting and highlight any remaining warnings or TODOs in the description.
