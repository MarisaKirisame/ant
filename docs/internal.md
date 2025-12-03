# Ant Architecture & Memoisation Guide

This document explains how the Ant compiler and runtime cooperate to implement
prefix memoisation for the CEK machine. It summarises the major OCaml modules,
their responsibilities, and the data structures that make reuse possible.

## Compiler Pipeline

1. **Parsing** – `Lexer.mll`, `Tokens.mly`, and `Parser.mly` translate source text
   into the algebraic syntax in `Syntax.ml`. Pretty-printers (`Syntax.pp_*`)
   make intermediate trees debuggable.
2. **Name resolution + typing** – `Typing.ResolveGlobal` promotes known names to
   `GVar` before Hindley–Milner inference in `Typing.ml`/`Type.ml` annotates the
   program. Printers exposed through the CLI show either plain or level-aware
   types (`-t`, `-L`).
3. **Normalisation** – `Transform.ml` performs CPS and defunctionalisation;
   `Pat.ml` lowers pattern matches into decision trees.
4. **Backends** –
   - `CompileMemo.ml` emits the memoising CEK VM.
   - `CompileSeq.ml` emits a pure interpreter for comparison runs.
   - `CompilePlain.ml` renders direct OCaml for quick inspection.
   All backends rely on the IR helpers in `Code.ml`/`Ir.ml`, and
   `CompileType.ml` auto-generates OCaml ↔ runtime converters for ADTs.
5. **Drivers** – `bin/main.ml` wires the switches together. `make run` rebuilds
   generated modules under `generated/`, formats the tree, and executes
   `GeneratedMain`.

## Runtime Representation

### Words, Values, and Patterns (`Words.ml`, `Value.ml`, `Pattern.ml`)
- `Word.t` encodes integers and constructor tags; `Words.t` stores a prefix
  traversal of user values as a finger tree annotated with length, degree, and
  CRC32C hash.
- `Value.t` extends `Words` with `Reference` nodes that defer slices of the
  environment or continuation. Its measure tracks both total degree and whether
  a fully hashed prefix is available.
- `Pattern.t` represents “holes plus concrete prefixes”. Patterns fuse adjacent
  holes/constructors and carry hole counts so they can match or compose states
  in logarithmic time.

### State (`State.ml`)
- A CEK state is `{ c; e; k; sc }` (control, environment dynarray, continuation,
  and step count). `world` pairs a state with the memo store and bookkeeping for
  which slots were fully resolved during a step.

## Memo Storage & Dependency Composition

Ant currently memoises *steps* rather than whole functions:

- Each observed transition is recorded as `{ src; dst; sc }`, where `src` is a
  pattern over the CEK state and `dst` is the resulting concrete state.
- `Dependency.make_step` builds `src` by marking the parts of the environment or
  continuation actually inspected during the step as concrete, leaving the rest
  as holes. It also captures how many VM steps (`sc`) were executed.
- `Memo.exec_cek` tries to match the current state against recorded `src`
  patterns (`Dependency.can_step_through`). On a hit, it substitutes the live
  bindings and jumps via `Dependency.step_through` instead of interpreting.
- A binary counter of “history slices” composes adjacent steps using
  `Dependency.compose_step`, growing longer reusable fragments whenever two
  neighbouring steps align.
- Steps that cannot be matched simply execute once and are then added to the
  memo list for future runs.

This design keeps the memo store linear and relies on pattern unification to
generalise reuse across different concrete inputs.

## Generated Modules

- `generated/TestSeq.ml`, `generated/TestCEK.ml`, and `generated/TestPlain.ml`
  come from `examples/Test.ant` and mirror the outputs of the CLI backends.
- `generated/Live*.ml` host hand-written CEK demos (live editing, tail recursion
  examples) used by `GeneratedMain`.
- All files under `generated/` are overwritten by `make run`.

## Testing & Benchmarking

- `test/test_ant.ml` exercises monoid hash implementations and the custom
  integer map (`Intmap.ml`).
- Microbenchmarks in `bench/*.ml` compare hashing and list operations; run with
  `dune exec bench/<name>.exe`.

## Further Work

- Document the CPS/defunctionalisation pipeline in more depth (closure capture,
  free-variable handling) within `Transform.ml`.
- Add regression tests that exercise step composition (`Dependency.compose_step`)
  and the memo fast-forward path in `Memo.exec_cek`.
- Explore pruning or indexing strategies for the step list to keep lookup costs
  predictable on larger programs.
