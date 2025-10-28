# Codex Agent Notes

These notes document how the Codex agent currently works in this repository so you know what to expect (and what to avoid) when asking it for help.

## Capabilities
- Runs inside the repository root `ant`.
- Executes shell commands via `make`, `dune`, `opam`, etc., using `bash`.
- May create and modify files directly (prefers `apply_patch` for small edits).

## Limits
- `make build|run|dependency` trigger `nightly.py`, which still touches the opam switch named `ant`; expect the harmless “already installed switch” warning.
- Long `make` runs can timeout at 30 s unless explicitly requested with a longer timeout.
- OCaml warnings (pattern-match exhaustiveness, unused rec) currently surface during builds; they are known noise unless you ask otherwise.

## Typical Workflow
1. Inspect files or use `rg`/`sed` for context.
2. Modify code with `apply_patch`.
3. Run `make build` (fast, compiles) or `make run` (build + execute) as verification.
4. Summarise changes referencing file paths and lines when responding.

## Tips for Requests
- Be explicit about targets (files, functions, behaviour).
- Mention if you want tests or runs skipped; otherwise the agent will try to execute the relevant `make` target.
- For formatting-only runs, prefer `make run` (already calls `dune fmt --display=quiet`).
- If you add or remove modules, update `lib/dune` accordingly; the agent will check this automatically.

## Task Handoff
- After edits, run `git status -sb` to confirm expectations.
- Use `make build` to sanity-check compilation.
- Describe remaining TODOs or manual steps in your final message if any exist.
