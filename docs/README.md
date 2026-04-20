# Ant

Ant is an experimental incremental evaluator that memoises *prefixes* of CEK machine executions.
Instead of caching whole calls, Ant records reusable fragments of the control–environment–continuation state so later runs can jump over work when the live input shares that prefix.

## How It Works
- **CEK machine core.** Programs are compiled to a control table; the evaluator runs a CEK machine `(C, E, K)` and records step summaries keyed by program counter plus slices of the environment/continuation.
- **Finger-tree values.** Environments and continuations are finger trees annotated with monoid hashes and degree measures. This lets Ant split or compare prefixes without rescanning full structures (see `internal.md`).
- **Dependency-aware skips.** Each executed step becomes a pattern–value pair. If the current state matches a recorded source pattern, Ant substitutes the live bindings, composes steps, and fast-forwards instead of re-stepping the VM.

For deeper dives, see `internal.md` (architecture) and `motivation.md` (why this approach exists).

## Architecture at a Glance
- **Front-end.** `Lexer.mll`, `Parser.mly`, and `Syntax.ml` define the surface language; `Typing.ResolveGlobal` upgrades known names to globals so typing can treat them specially.
- **Type inference.** `Typing.ml` implements a constraint-based Hindley–Milner engine over the `Type`/`SynInfo` representations.
- **Normalisation.** `Transform.ml` performs CPS and defunctionalisation; `Pat.ml` lowers pattern matches into decision trees.
- **Backends.** `CompileMemo.ml` emits the memoising CEK VM, `CompileSeq.ml` emits a pure interpreter, and `CompilePlain.ml` produces direct OCaml for quick inspection. All three share the IR helpers in `Code.ml`/`Ir.ml`; `CompileType.ml` generates OCaml converters for user-defined ADTs.
- **Runtime core.** Finger-tree values live in `Value.ml`/`Words.ml`; memo storage and step composition are implemented in `Memo.ml`, `Dependency.ml`, `Pattern.ml`, and `State.ml`.

## Getting Started

use `uv sync` to install deps.

```bash
make dependency   # install toolchain/dev tools and refresh dune.lock
make build        # dune build
make run          # regenerate generated/*.ml, format, and execute GeneratedMain
```

you may need `git submodule update --init`

`nightly.py` underpins these targets; it will print a harmless warning if the `ant` opam switch already exists.

## Repository Layout
- `lib/` – OCaml sources (front-end, backends, memo/runtime infrastructure).
- `bin/` – CLI entrypoints plus hand-written experiment runners used by `GeneratedMain`.
- `generated/` – Auto-generated OCaml modules (rebuilt by `make`).
- `examples/` – Sample `.ant` programs; `examples/Test.ant` is the default demo.
- `bench/` – Benchmarks.
- `docs/` – Project documentation (`README.md`, `internal.md`, `dependency.md`, `motivation.md`, `AGENTS.md`).

## Command-Line Interface
Run `dune exec ant -- INPUT OUTPUT [flags]` to compile or inspect a program. The output file collects whatever artifacts you request.

- `-p`, `--print-ast` – pretty-print the resolved surface syntax.
- `--pat`, `--compile-pat` – dump the compiled pattern matrices from `Pat`.
- `--compile` – emit backend output (`CompileMemo` by default) to the output file.
- `-b`, `--backend memo|seq|plain` – pick the backend for `--compile`.
- `-t`, `--typing` – print inferred types from `Typing`; combine with `-L` to show levels.
- `-c`, `--print-cps` – show the CPS-transformed AST.
- `-d`, `--print-defunc` – show the defunctionalised AST.
- `-D`, `--print-cps-defunc` – run CPS then defunctionalise before printing.

All CLI wiring lives in `bin/main.ml`.

## Development Workflow
- `make dependency` – create/update the local `ant` opam switch, install the Dune toolchain/dev tools, and refresh the lockdir with `dune pkg lock`.
- `make build` – build through the managed switch (`dune build`).
- `make run` – regenerate `generated/*.ml`, format, and execute `GeneratedMain`.
- `dune runtest` – run unit tests in `test/test_ant.ml` (hash monoid and intmap checks).
- `dune exec bench/<target>.exe` – run benchmarks in `bench/`.

Generated modules under `generated/` are overwritten by `make run`; avoid editing them manually.

## Contributing
Pull requests are welcome. Please run `make build` before submitting and note any remaining warnings or TODOs in the description.
