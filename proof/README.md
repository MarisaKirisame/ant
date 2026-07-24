# Agda Mechanization

This directory contains the mechanization of the theorems in “Incremental Live
Programming via Shortcut Memoization.”

The proof is pinned to Agda 2.7.0.1 and Agda standard library 2.2 in the
artifact Docker image. From the repository root, build and check it with:

```sh
make proof
```

`all.agda` imports all proof modules:

- `core.agda` contains Definitions 2.1–2.5 and 2.8.
- `compatibility.agda` contains Theorem 2.6 (Compatibility of Rule
  Composition).
- `existence.agda` contains Theorem 2.7 (Shortcut Existence).
- `validity.agda` contains Theorem 2.9 (Shortcut Validity).

The development postulates function extensionality, a generic constructor set,
and an infinite variable set with the operations and decidable equality
specified in `core.agda`.
