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

## CompileMemo Backend

`CompileMemo.ml` builds the specialised CEK VM in three layers:
- **IR helpers** – The `Code` module offers a small DSL for documents; all code
  emission should go through these helpers rather than raw string
  concatenation.
- **Generation combinators** – Scope/continuation registries track constructor
  tags, environment layouts, and the code fragments registered for each
  continuation.
- **Printers** – `compile_*`, `generate_apply_cont`, and `pp_cek_ant` traverse
  syntax/IR to produce OCaml source, accumulating snippets in `codes` for final
  emission.

Temporary values are stored in the environment whenever possible to avoid
interpretative overhead from the continuation stack `K`; we only use `K` for
non-tail calls.

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

### Monoid Parsing (`Words.ml`)

Ant treats values as strings via their prefix traversal (e.g. `Cons 0 (Cons 1
Empty)` becomes `Cons 0 Cons 1 Empty`). Constructors have degrees of
`1 - arity`; atomic values have degree `1`. A substring’s degree is the sum of
its parts, so the first prefix that reaches degree `1` marks a complete value.
Substrings can therefore represent whole values, several adjacent values, or
incomplete prefixes (negative degree). Pattern matching can select a branch
after seeing only the first word, deferring constructor arguments as
references.

### Patterns as Finger Trees (`Pattern.ml`)

Patterns reuse the finger-tree machinery so “holes plus concrete prefixes” can
be split and fused in `O(log n)`. The measure tracks degree, `max_degree`, and
hole counts, allowing adjacent constructors or holes to be merged. During memo
composition, `compose_pattern` rehydrates concrete values from the bindings
produced while matching.

### Value Slicing Semantics (`Value.ml`)

`pop_n` splits a value sequence by *degree* rather than by finger-tree nodes. It
finds the shortest prefix whose `max_degree` meets the requested count, returns
that prefix (including the boundary element), and leaves the remainder. If the
boundary is a `Reference`, only the needed portion of its degree is consumed.

### CEK State Representation (`State.ml`)

`exp` is intentionally not an ADT. Encoding the control component as a variant
would add overhead, and it is unclear how to incrementalise recursive functions
via such an ADT; instead memoisation focuses on CEK steps and the resolved
environment/continuation slices captured per step.

### Pattern Matrix Debug Output (`Pat.ml`)

`pp_pattern_matrix` renders the lowered decision table for debugging:

```
arity = 4
occurrence = [.0, .1, .2, .3]

(PAny, PAny, PAny, PAny)   -> 1 with { .0 -> x, .1 -> y, .2 -> z, .3 -> w }
(PInt 1, PAny, PAny, PAny) -> 2 with { }
(PInt 2, PAny, PAny, PAny) -> 3 with { .0 -> z }
(PInt 3, PAny, PAny, PAny) -> 4
(PInt 4, PAny, PAny, PAny) -> 5
```

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
