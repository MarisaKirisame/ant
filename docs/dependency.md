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

1. Track which environment/continuation entries were fully resolved while
   executing the step (the `resolved` flag in `State.world`).
2. Convert the pre-step state into a pattern: resolved slots become concrete
   prefixes, unresolved slots become holes sized by their degree.
3. Run the step once more to capture the concrete post-state as `dst` and record
   the step count `sc`.

## Matching and Fast-Forwarding

- A memo hit occurs when `value_match_pattern_ek` can align the current state
  with a recorded `src`. The alignment produces a substitution map from holes to
  concrete slices.
- `step_through` applies that substitution to the recorded `dst` and advances the
  step counter by `sc`, effectively skipping interpretation for the matched
  fragment.

## Composing Steps

To grow reusable fragments, `Memo.exec_cek` keeps a binary counter of recently
executed slices. Whenever two neighbouring slices share a program counter,
`Dependency.compose_step` attempts to fuse them:

1. Unify the destination of the first step with the source pattern of the second
   to derive a common set of holes.
2. Apply the resulting substitutions to build a generalised source and run the
   two steps back-to-back, producing a composed `{ src; dst; sc }` with
   `sc = sc_x + sc_y`.
3. Insert the composed step into the memo list for future reuse.

## Limitations and Next Work

- The memo store is a flat list; lookup is linear in the number of steps. An
  index or eviction policy will eventually be needed for larger programs.
- Composition currently requires matching program counters; broader partial
  overlaps are not yet exploited.
- Resolved-slot tracking is coarse-grained (per environment/continuation entry),
  so very fine-grained dependencies may still force re-execution.
