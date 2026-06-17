# Dependency Tracking

This note describes how Ant currently records and replays dependencies inside
the memoising CEK machine. The implementation lives primarily in
`Dependency.ml`, `Pattern.ml`, and `Memo.ml`.

## What We Record

- **Step summaries** – Each executed transition becomes `{ src; dst; sc }` where
  `src` is a pattern over the CEK state (control, environment, continuation) and
  `dst` is the resulting concrete state after `sc` VM steps.
- **Patterns with holes** – A pattern keeps concrete prefixes for any slot the
  VM inspected during the step and uses sized holes for everything else. Adjacent
  holes/constructors are fused so matching stays logarithmic in the finger-tree
  size.

## Building a Step (`Dependency.make_step`)

During the execution, the code only resolves a value (via `Dependency.resolve`)
when attempting to observe (e.g. pattern matching) it, these information will be
recorded in the `resolved` flag in `State.world`.

`make_step` will first convert the pre-execution state (`src`) into pattern,
based on the `resolved` flag. An unresolved value is abstracted as a pattern
sized by their degree, and a resolved value become concrete prefix.

After conversion, `make_step` will run the code once again on pattern to compute
the pattern of post-state as `dst` and record the step count `sc`.

As an example, consider following rewrite rule

``` text
Recurse (Pair (S Z) l) k -> Recurse l (Cons1 k)
```

Before apply the rules, the current state of CEK machine is like

``` text
$0: Z (Resolved)
$1: S Z (Resolved)
$2: l (Unresovled)
$3: Pair (S Z) l (Resolved)
$K: k (Unresovled)
```

Since `$2` and `$K` are both unresolved, they're assigned to fresh pattern
variables `?B` and `?K`.

``` text
$0: Z
$1: S Z
$2: ?B
$3: Pair (S Z) l
$K: ?K
```

`$0`, `$1` and `$3` are resolved, we preseve its constructor prefix and
introducing fresh pattern variable for its child component. Noted that `Z` is a
nullary constructor thus no pattern variable will be assigned.

``` text
$0: Z
$1: S ?A
$2: ?B
$3: Pair ?C
$K: ?K
```

Then left-hand side of rewrite rule is reformulated as pattern `Recurse (Pair (S
Z) ?B) ?K`. After re-execute the code on the pattern, we get the rewrote form as
`Recurse ?B (Cons1 ?K)`.

## Matching and Fast-Forwarding

- A memo hit occurs when `value_match_pattern_ek` can align the current state
  with a recorded `src`. The alignment produces a substitution map from holes to
  concrete slices.
- `step_through` applies that substitution to the recorded `dst` and advances the
  step counter by `sc`, effectively skipping interpretation for the matched
  fragment.

## Composing Steps (`Dependency.compose_step`)

To grow reusable fragments, `Memo.exec_cek` keeps a binary counter of recently
executed slices. Whenever two neighbouring slices share a program counter,
`Dependency.compose_step` attempts to fuse them:

1. Unify the destination of the first step with the source pattern of the second
   to derive a common set of holes.
2. Apply the resulting substitutions to build a generalised source and run the
   two steps back-to-back, producing a composed `{ src; dst; sc }` with
   `sc = sc_x + sc_y`.
3. Insert the composed step into the memo list for future reuse.

Consider two adjacent steps `x` and `y` which represents following rewrite rules

``` text
Recurse (Pair (S Z) ?l1) ?k1 -> Recurse ?l1 (Cons1 ?k1)

Recurse (Z ?l2)          ?k2 -> Recurse ?l2 (Cons0 ?k2)
```

It's not hard to see that pattern `Recurse ?l2 (Cons1 ?k1)` (`x.dst`) and
`Recurse (Z ?l2) ?k2` both matches on the same state. Therefore we use
unification algorithm to calculate the substitutions for `x.src` and get
`?l1 -> (Pair Z ?l2)`

Noted that we don't calculate substitutions for `Recurse ?l2 (Cons0 ?k2)`
(`y.dst`), instead, it was dervied via applying step `x` and `y` again on the
generalized input `Recurse (Pair (S Z) (Pair Z ?l2))`.

## Limitations and Next Work

- The memo store is a flat list; lookup is linear in the number of steps. An
  index or eviction policy will eventually be needed for larger programs.
- Composition currently requires matching program counters; broader partial
  overlaps are not yet exploited.
- Resolved-slot tracking is coarse-grained (per environment/continuation entry),
  so very fine-grained dependencies may still force re-execution.
