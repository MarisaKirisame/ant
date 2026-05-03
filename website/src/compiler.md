# Compiler Use

Run the compiler through Dune:

```bash
dune exec \Ant -- INPUT OUTPUT [flags]
```

Common flags:

| Flag | Effect |
| --- | --- |
| `--compile` | Emit generated OCaml. |
| `--backend memo` | Use the memoised CEK backend. This is the default. |
| `--backend plain` | Emit direct OCaml for inspection. |
| `--backend seq` | Emit sequence-oriented backend output. |
| `--print-ast` | Pretty-print the parsed and typed surface program. |
| `--typing` | Print inferred top-level types. |
| `--compile-pat` | Dump pattern compilation data. |

Example:

```bash
dune exec \Ant -- examples/Arith.ant generated/ArithCEK.ml --compile --backend memo
```

The repository Makefile keeps the documentation workflow separate from the compiler workflow:

```bash
make website
make website-check
make website-serve
```

Set `ANT_DOC_NAME` when rendering the same book with a different name:

```bash
ANT_DOC_NAME=MiniAnt make website
```
