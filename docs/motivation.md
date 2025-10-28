# Why Ant Exists

Ant is a research vehicle for incremental evaluation.  It started with a simple
observation: conventional memoisation is great at reusing *entire* function calls,
yet almost every real computation repeats large *prefixes* instead.  Compilers,
build systems, static analysers, IDE tooling, data pipelines—each spends most of
its time re-walking a shared prefix of work after a tiny edit near the end.  We
want a runtime that can recognise and skip those repeated stretches automatically.

## The Pain Today

- **Poor granularity.**  Hash consing or function-level memo tables only help when
  the whole call graph matches bit-for-bit.  Appending a single token to the input
  forces a full recompute.
- **Expensive change detection.**  Many incremental systems re-run static analyses
  (dependency discovery, diffing, top-down re-execution) before they can reuse a
  result.  That bookkeeping often dwarfs the work we hoped to skip.
- **Complicated developer experience.**  Retrofitting a system for incremental
  execution usually means rewriting it in a domain-specific dataflow language—or
  littering the code with caches, invalidation logic, and debug-only logging.

## Ant’s Core Idea

Memoise *prefixes* of CEK machine executions rather than whole calls:

1. Compile high-level expressions into a control table: each step of the CEK
   machine is a numbered program counter.
2. Encode environments and continuations as finger trees with monoid measures
   that capture length, degree (how “complete” a value is), and hashes.
3. Store reusable fragments `F[XS] -> G[XS]` in a memo trie keyed by the control
   location plus the measured slices of the environment/continuation.
4. When execution revisits the same prefix, splice the live bindings directly into
   `G` and fast-forward without replaying the intermediate steps.

The runtime does not guess: every memoised skip corresponds to an actual execution
fragment it has already witnessed.  If the lookup fails, the VM simply continues
stepping normally, expanding the memo table as it goes.

## Why It Matters

- **True interactive responsiveness.**  Prefix memoisation targets edits at the end
  of a workflow—exactly the kind that make IDEs or live compilers feel sluggish.
- **Predictable overhead.**  The finger-tree representation lets us compare and
  split values in `O(log n)` time; we pay for reuse only where reuse is possible.
- **Language-agnostic runtime.**  The CEK machine is generated; Ant’s approach can
  be applied to any language we can lower to the control table.
- **Research-friendly platform.**  The project surfaces every intermediate: AST,
  CPS, defunctionalised forms, sequential interpreter, memoising VM.  That makes it
  easy to experiment with new optimisations or to instrument the memo table.

## What Makes Ant Different

- **Prefix-sensitivity over exact equality.**  Traditional memo systems “all or
  nothing” reuse.  Ant’s finger trees allow it to recognise and reuse partial
  matches on demand.
- **Runtime-guided structure synthesis.**  The memo trie records what the evaluator
  actually needed; there is no up-front dependency discovery pass.
- **Composable hashing.**  The monoid hash layer means large prefixes can be
  compared without re-reading every word, keeping lookup latency low.

## Where We’re Heading

1. **Tighter IDE integration.**  Hook Ant into editors to benchmark latency on live
   code reorganisations.
2. **Language back-ends.**  Extend the generator so more front-ends can target the
   memoising CEK VM.
3. **Adaptive memo policies.**  Explore heuristics that decide when to persist or
   evict fragments based on observed hit rates.

If you care about making “instant feedback” the default for large-scale programs,
Ant is the sandbox where we are trying to make it happen.  Dive in, experiment,
and tell us which incremental workloads you want to see accelerated next.
