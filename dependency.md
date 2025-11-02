# Dependency Tracking

This note records how Ant currently tracks dependencies inside the memoising
CEK machine, why the strategy struggles on larger programs, and the revised
approach we want to implement.  The focus is on keeping the memo trie honest
about which slices of state were inspected so that we can replay execution
fragments safely.

## Terminology

- **State decomposition** – We model an execution state as `F[X]`, where `F`
  denotes the memoised prefix we hope to reuse and `X` is the residual slice
  that remains generalised.  When we run a step, the state transitions from
  `F[X]` to `G[X]`.
- **References** – When a value is not fully materialised, it is represented by
  a reference pointing back to a source slice (environment slot, store entry, or
  continuation).  Resolving a reference records a dependency.
- **Dependency tree** – A hybrid between a radix trie and a rope.  It stores
  alternating stretches of concrete sequences and “holes” (value counts) while
  respecting ADT structure so that constructor nodes always align with their
  arguments.

## Current Dependency Tracking

The present system tries to *guess* the reusable prefix `F`.  We begin with an
empty prefix so everything lives in `X`.  Whenever execution needs to touch
`X`, we promote the accessed location into `F`.

- Promotions happen at exponentially increasing lengths: if we inserted only the
  exact element that was read, we would explode the number of memo trie nodes and
  spend most of the time walking the trie.  By promoting larger prefixes we hope
  subsequent runs will reuse the cache.
- The approach breaks down on programs that explore new regions of the state
  gradually.  We either guess too cautiously (causing repeated cache misses) or
  too aggressively (bloating the trie and losing the benefit of memoisation).

In short, the system depends on the quality of the heuristic that guesses `F`,
and the heuristic is brittle.

## New Recording-Based Approach

Instead of guessing we record what actually happened.  Each execution trace from
state `A` to `B` yields a pair `A = F[X]` and `B = G[X]`, where `X` captures all
the holes we observed.  When we compose traces we rely on explicit substitutions
rather than implicit guesses.

### Computing `GK`

When composing two steps `(F[X] -> G[X])` and `(H[Y] -> I[Y])`, we need a view of
the intermediate state that aligns the holes in `G` with the materialised data
in `H`.  The practical recipe:

- Walk `G` and `H` in lockstep.  Whenever `G` yields a reference but `H` offers a
  concrete prefix, record the substitution `X ↦ value-with-holes`.
- When both sides still expose references, do nothing; we rely on later
  executions to flesh out that part of the tree.
- The resulting substitution map defines `GK`, the specialised version of
  `G` in which every resolvable hole has been filled with the observed slice.

We do **not** compute an explicit `HK`.  Once `GK` is known we can replay the two
steps with substitutions applied, and the runtime will naturally produce the
equivalent `HK`.

### Composing Steps

Given the substitution map, we can phrase both steps in terms of the same set of
holes:

```
F[GK[_]] -> G[GK[_]]
G[GK[_]] = H[HK[_]]
H[HK[_]] -> I[HK[_]] -- HK derived by rerunning with substitutions
-------------------------------------
F[GK[_]] -> I[HK[_]]
```

Composition therefore consists of:

1. Aligning prefixes via the dependency tree so that constructor boundaries stay
   in sync.
2. Applying the substitution map to regenerate `F[GK[_]]` and to feed the second
   step the material it expects.
3. Recording any newly resolved references while replaying the composed trace,
   which may add larger slices to the dependency tree.

### Integrating With the Dependency Tree

- **Structure awareness** – Because the tree stores ADT structure explicitly, we
  can intersect two traces even when they look at different subtrees.  When
  overlaps occur we split nodes so that each branch maintains consistent
  boundaries.
- **References as holes** – The tree treats unresolved regions as hole nodes with
  a value count.  References in future runs point directly to these holes instead
  of ranging over arbitrary offsets.